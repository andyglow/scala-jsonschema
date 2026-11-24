# `-Xlint` warns "Implicit resolves to enclosing value" on `implicit val s: Schema[X] = Json.schema[X]`  (issue #320, item B9)
Status: draft

## Problem

The README's basic usage warns on Scala 2.13 with `-Xlint` (or just `-Xlint:implicit-recursion`):

```scala
object Test {
  case class UserId(value: String) extends AnyVal
  implicit val userIdSchema: json.Schema[UserId] = Json.schema[UserId].toDefinition("userId")
}
```
```
[warn] Test.scala:3:63: Implicit resolves to enclosing value userIdSchema
[warn] Applicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=w-flag-self-implicit, site=Test.userIdSchema
```

Measured on master (`4b2aff3`), `sbt "++2.13.18" clean Test/compile` with `-Xlint:implicit-recursion` added to every module:
**35 warnings**, all in our own tests (api 32, u-json 3; 2 of them at `<macro>` positions, from `assertCompiles` snippets in
`SchemaMacroNamesSpec`). Every one is the right-hand side of the implicit being defined:

| shape | example (test file) |
|---|---|
| `implicit val s: Schema[X] = Json.schema[X]` in an object/class, also with `.toDefinition` | `Example.scala:71`, `DefinitionsSpec.scala:128` |
| the same in X's companion (found through the implicit scope, not the lexical one) | `AutoDerivationSpec.scala:28`, `CRLFSourceSpec.scala:48` |
| spread over several lines | `SelfReferenceSpec.scala:89` |
| `implicit val/def p: Predef[X] = Predef(Json.schema[X])` | `SelfReferenceSpec.scala:64, 68` |
| a generic `implicit def f[T](implicit t: Schema[T]): Schema[F[T]] = Json.schema[F[T]]` | `RecursiveTypesSpec.scala:124` |

Versions: Scala 2.13 only. 2.12 has no such lint; Scala 3.3.9 has none either (`scalac -W` lists no self-implicit check), and
Scala 3's derivation has no source-text heuristic. JVM and JS are the same (it's a compile-time warning).

The cause. For each type the macro searches an implicit `Schema[T]`, then `Predef[T]` (the core rule). The search runs in the call
site's context, whose owner is the val being defined, so for the val's own type it finds the val itself. The macro then declines it
(a self reference is still `null` when the derived code runs), using two checks in `macros/.../UImplicits.scala`:
- `isSelfRef`, from 0.7: reads the source text of the previous and the current line and calls the found implicit a self reference if
  that text contains its name, `=` and `implicit`;
- `isOwnerSelfRef`, from 0.8.0: the found symbol is one of `c.internal.enclosingOwner`'s owners (or their getter), as on Scala 3.

But scalac reports the warning inside the search (`Implicits.scala` in 2.13.18, after a successful search: "if the result's symbol, or
the field it accesses, is an owner of `context.owner` ... `context.warning(..., WFlagSelfImplicit)`"). By the time the macro declines,
the warning is already out. Under `-Werror` (sbt-tpolecat's CI mode; its default options include `-Xlint:implicit-recursion`), the README's
first example doesn't compile.

The source-text heuristic also has a false positive, documented in README "Known issues" ("an implicit declared on the line just
before a `Json.schema` call may be ignored") and pinned by `EntryPointsCharSpec` R4 (TRIAGE-A):
```scala
implicit val barSchema: Schema[Bar]       = Json.schema[Bar]
implicit val hasBarSchema: Schema[HasBar] = Json.schema[HasBar]   // Scala 2: Bar inlined, barSchema ignored; Scala 3: $ref to Bar
```
The comments on #320 list two more failures of that heuristic (a comment inside the type, CRLF line endings: `NullPointerException`
at runtime, and a typed local implicit in a method: "forward reference"). Those are already fixed by `isOwnerSelfRef` in 0.8.0
(verified on 2.12.21: the local implicit compiles and gives `def(signature = userId, schema = string)`).

## Why it is a problem

- The first thing the README shows warns under a standard lint, and fails a `-Werror` build (#320, one user report; any sbt-tpolecat user).
- Users can't fix it without silencing the lint for the whole site (`@nowarn`, `-Wconf`), which also hides real implicit recursions.
- The heuristic behind it silently drops a user's implicit when two implicits sit on adjacent lines (README known issue).

## Options

### A. Leave the enclosing definitions out of the search (`c.inferImplicitValueIgnoring`, 2.13.17+) — WON

Scala 2.13.17 added `c.inferImplicitValueIgnoring(pt)(symbols: _*)` to the macro API (checked: absent from scala-reflect 2.13.16 and
2.12.21, present in 2.13.17 and 2.13.18). The macro already computes the enclosing owners; it passes them (plus a field's getter, and
an object's module symbol) as the ignored set. The search never finds the val being defined, so scalac has nothing to warn about, and
it returns what it would have found without the val, which is exactly what the macro wants (today it finds the val and then falls
through to the Predef search).

- `macros/src/main/scala-2.13/.../ImplicitSearch.scala`: `inferIgnoring(c)(pt, ignored): Option[Tree]`, calling
  `c.inferImplicitValueIgnoring`. The macro runs on the *user's* scala-reflect, so a 2.13.0–2.13.16 compiler fails to link the call
  (`NoSuchMethodError`): catch `IncompatibleClassChangeError`, remember it, and return `None` (the caller does the plain search).
- `macros/src/main/scala-2.12/.../ImplicitSearch.scala`: always `None` (no API, and no lint to avoid).
- Exact-binary dirs `scala-2.12`/`scala-2.13`, not the repo's usual `scala-2.12-`/`scala-2.13+`: `scala-2.13+` is also compiled on
  Scala 3, where macros has no scala-reflect. Nothing has to be copied for Scala 3 (its macro is elsewhere), which was the reason for
  the convention.
- `UImplicits`: both searches (`Schema[T]` and `Predef[T]`) go through it. `isOwnerSelfRef` stays as the safety net for 2.12 and old 2.13.
- The `Json.auto.derived` re-search (a typechecked `{ val derived = (); implicitly[Predef[T]] }`) can't use the ignoring API: the
  ignored set only applies to the top-level candidate, so nested searches expand `auto.derived` again (prototype: scalac
  `StackOverflowError` on 2.13.18). It keeps the typecheck and also shadows the enclosing definitions' names
  (`{ val derived = (); val p = (); implicitly[...] }`). Without that, `import Json.auto._` plus
  `implicit val p: Predef[X] = Predef(Json.schema[X])` still warns (prototype: yes, `Implicit resolves to enclosing value p`).

Prototype (patch on master): with the lint turned on as an error, 0 warnings; every JVM test passes on 2.13.18 and 2.12.21, no golden
file changes. With `isSelfRef` also removed (below), the only test that changes is R4.

Pros: no user change; the search is what scalac itself would do without the self reference, so no guessing; same result as Scala 3.
Cons: only on 2.13.17+ compilers. Users on 2.13.0–2.13.16 keep today's warning (documented, with the `-Wconf` filter). Two small
version-specific files.

### B. Don't search when an enclosing implicit already provides the type — LOST

Before searching `Schema[T]` (or `Predef[T]`), look at the enclosing owners: if one is an implicit val/def whose result type conforms to
`Schema[T]`, skip the search (go on to `Predef[T]`), as if it had been found and declined. No new API: works on every 2.13 compiler.

Cons: it's our guess at what scalac would have picked. If a more specific implicit is also in scope (say an imported
`Schema.object[X]`), today's search returns that one, and B would skip it: a silent output change. It also has to mirror scalac's
rules for getters, lazy vals, modules and generic defs. A asks the compiler and is exact. Lost on correctness; it would be the
fallback if 2.13.0–2.13.16 mattered more (it could be added for them later).

### C. Shadow the enclosing name in a typechecked search — LOST

What the macro already does for `Json.auto.derived`: typecheck `{ val userIdSchema = (); implicitly[Schema[UserId]] }`. A local
non-implicit name makes an in-scope implicit of that name ineligible. Works on every Scala 2 version.

Cons: it only hides implicits found in the lexical scope. An implicit in X's companion is found again through X's implicit scope
and warns as before: 4 of our 35 (`AutoDerivationSpec`, `CRLFSourceSpec`, `TriageBPendingSpec`, `BacktickedModels`), and the
companion is the usual home for such an implicit. It also typechecks a tree for every lookup. (A uses it only for the `auto.derived`
corner, where A's API can't help.)

### D. Document a suppression — LOST (kept as the note for old compilers)

README: add `-Wconf:cat=w-flag-self-implicit:s` (or `@nowarn("cat=w-flag-self-implicit")` on the val). The user changes their build,
and it hides real recursions too. Not a fix; the README gets it only as the workaround for 2.13.0–2.13.16.

### The source-text heuristic `isSelfRef`: remove (lead's go-ahead, 2026-11-23) — or keep

With A the true self reference is never returned on 2.13.17+, and `isOwnerSelfRef` catches it on 2.12 / old 2.13 (it already does for
every case `isSelfRef` sees: the found val is always an owner of the expansion). What `isSelfRef` still does is its false positive.
- **Remove (chosen).** Scala 2 then matches Scala 3: adjacent implicits work (R4), and a declined self `Schema` falls through to a
  `Predef` of the type on every line layout (today a definition on one or two lines skips the Predef, one on three or more uses it:
  `SelfReferenceSpec` "falls through to a Predef ... (reviewer9)"). Same on 2.12, which has no new API.
- **Keep.** No output change at all, the README known issue stays. It's a derivation-time check, so a profile (Legacy) couldn't keep it
  per profile anyway; under the lead's Legacy policy this is a bug, not a style.

## Decision

A, plus removing `isSelfRef`. The macro's implicit searches leave out the definition being expanded (2.13.17+), so `-Xlint` has
nothing to report and nothing changes in the output; older compilers keep today's search. The source-text heuristic goes, which
fixes the README known issue on every Scala 2 version.

## Implementation plan

1. `macros/src/main/scala-2.13/com/github/andyglow/jsonschema/ImplicitSearch.scala` and `.../scala-2.12/...` as in A.
2. `macros/src/main/scala-2/.../UImplicits.scala`: `selfSymbols` (enclosing owners + getters + module symbols), `infer(pt)` =
   `ImplicitSearch.inferIgnoring(c)(pt, selfSymbols) getOrElse c.inferImplicitValue(pt)` for the `Schema` and the `Predef` search;
   `selfNames` shadowed in the `auto.derived` re-search; remove `isSelfRef` (and `LF`/`CR`, the `NoSourceFile` import); comments.
   The other `c.inferImplicitValue` calls (`Flag`, `ToValue`, `KeyPattern`) search types nobody defines with `Json.schema`: unchanged.
3. `project/CompilerOptions.scala` (already in the repro commit): 2.13 gets `-Xlint:implicit-recursion` and
   `-Wconf:cat=w-flag-self-implicit:e`, so a regression fails the 2.13 build (main and test code of every module).
4. Tests (shared `api/src/test/scala`, run on 2.12, 2.13, 3):
   - `SelfImplicitLintSpec` (repro commit): the README pattern with `toDefinition`, a comment inside the type, a companion implicit,
     a generic implicit def. Add: `import Json.auto._` + a self `Predef`; a two-line self `Schema` next to a `Predef` of the type
     (→ the Predef, on all versions).
   - `EntryPointsCharSpec` R4: one expectation for both versions (adjacent = separated); drop the `TriageA` tag and the
     "KEEP THESE TWO LINES ADJACENT" comment's Scala 2 part (keep the models: they're the regression test).
   - Golden files: no change (prototype). golden-legacy: no change.
5. Manual check, recorded in the report: a scratch build on Scala 2.13.16 using the macros jar built on 2.13.18, to see the
   fallback (`IncompatibleClassChangeError` caught: compiles, warns as today, no crash).
6. README "Known issues": remove the "line just before" bullet; add one line: on Scala 2.13.0–2.13.16, `-Xlint` still reports
   `implicit val s: Schema[X] = Json.schema[X]` (the compiler can't leave the val out of the search); upgrade to 2.13.17+ or add
   `-Wconf:cat=w-flag-self-implicit:s`. Paradox: `scala3.md` points to the README for Scala 2 issues; check whether any page repeats
   the known issue (none found: `grep` for "previous line" / "line just before"). `makeDocs` gate if a paradox page changes.

## Compatibility

- Source/binary: no public API change (`ImplicitSearch` and `UImplicits` are `private[jsonschema]` macro internals). Patch (0.9.1).
- Compiler support: unchanged. 2.13.0–2.13.16 compilers take the plain search (today's behavior minus `isSelfRef`).
- Output, Scala 2 only (Scala 3 already does this), Neutral and every profile including Legacy:
  - an implicit `Schema[X]` declared on the line just before another `Json.schema` call that uses X is now a definition + `$ref`
    (was: X inlined, the implicit ignored);
  - `implicit val s: Schema[X] = Json.schema[X]` on one or two lines, with an implicit `Predef[X]` in scope, now uses the Predef
    (was: derived X, Predef ignored; three or more lines already used it).
  Both are the README's documented behavior and Scala 3's. No golden or golden-legacy file changes.
- Build: the 2.13 build reports `-Xlint:implicit-recursion` as an error (our build only; not published).

## Review log
