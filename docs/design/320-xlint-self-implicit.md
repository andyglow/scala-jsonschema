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

### A. Leave the enclosing definitions out of the search (`c.inferImplicitValueIgnoring`, 2.13.17+), keep today's winner — WON

Scala 2.13.17 added `c.inferImplicitValueIgnoring(pt)(symbols: _*)` to the macro API (checked: absent from scala-reflect 2.13.16 and
2.12.21, present in 2.13.17 and 2.13.18). The macro already computes the enclosing owners; it passes them (plus a field's getter, and
an object's module symbol) as the ignored set. The search never finds the val being defined, so scalac has nothing to warn about.

**But a search without the self reference is not today's search minus the warning** (B9-R1). Today the enclosing implicit is always
a candidate of the *lexical* scope (it's a local or a member of an enclosing class), and scalac only looks at the implicit scope
(companions) when the lexical scope has nothing. So today the self reference wins against a companion implicit, and against a
lexical one it beats by scalac's specificity rule (equal type, defined in a subclass); then the macro declines it and derives. Ignoring
it lets that loser win (r-b9's probes P1–P3, 2.13.18 with the bare ignoring search):

| case | master (2.12, 2.13), Scala 3 | bare ignoring search, 2.13.17+ |
|---|---|---|
| P1 a custom `Schema[X1]` in X1's companion; `object Mine1 { implicit val mine: Schema[X1] = Json.schema[X1] }` | derive | `def(X1, custom)` |
| P2 the same with `Predef[X2]` / `implicit val mine: Predef[X2] = Predef(Json.schema[X2])` | derive | custom |
| P3 `trait Low3 { implicit def low: Schema[X3] = custom }; object Mine3 extends Low3 { implicit val mine: Schema[X3] = Json.schema[X3] }` | derive | `def(X3, custom)` |
| P5 an imported `Schema.object[X5]` + `implicit val mine: Schema[X5] = Json.schema[X5]` | `def(custom)` | `def(custom)` |
| P6 the same with the `Schema.object[X6]` in the same object | `def(custom)` | `def(custom)` |

So A keeps the result `y` of the ignoring search only if it would have beaten the enclosing implicit in today's search; otherwise
it counts as a declined self reference (Schema search → Predef search; Predef search → derive), exactly as today. In `UImplicits`:
- *the enclosing candidates for `pt`*: the enclosing implicit vals/defs (a field's getter carries `implicit`; its info may not be
  completed yet, the field's is) whose result type conforms to `pt` (type parameters as wildcards). One whose info isn't complete has
  no result type and is still being typed: scalac's `isValid` doesn't consider it either.
- *`y` is in the lexical scope*: its reference is an `Ident`, a `Select(This(_), _)`, or a `Select(qual, _)` where `qual` is not one of
  the objects that make up `pt`'s implicit scope (the companions of `pt`'s parts and the objects they're nested in). Otherwise it came
  from the implicit scope, and today the self reference would have won.
- *`y` beats each candidate* by scalac's rule (`Implicits.improves` → `Infer.isStrictlyMoreSpecific`): one point for the strictly
  more specific type (types with their type parameters abstracted), one for being defined in a proper subclass (or companion), and
  `y` needs more points. A tie is an ambiguity today: also declined.
- When there is no enclosing candidate, `y` is the plain search's result.

Prototype v3 (`notes/scratch/c-b9/prototype-v3.patch`), 2.13.18: P1, P2, P3 derive, P5, P6 `def(custom)`, the same as master, 2.12
and Scala 3 (and P4, r-b9's neighbour finding, stays as on master).

The rest of A:
- `macros/src/main/scala-2.13/.../ImplicitSearch.scala`: `inferIgnoring(c)(pt, ignored): Option[Tree]`, calling
  `c.inferImplicitValueIgnoring`. The macro runs on the *user's* scala-reflect, so a 2.13.0–2.13.16 compiler fails to link the call
  (`NoSuchMethodError`): catch `IncompatibleClassChangeError` (covers `AbstractMethodError` too), remember it in the object (once per
  macro classloader, not per expansion), and return `None`: the caller does the plain search, as today.
- `macros/src/main/scala-2.12/.../ImplicitSearch.scala`: always `None` (no API; 2.12's `-Ywarn-self-implicit` isn't part of `-Xlint`).
- Exact-binary dirs `scala-2.12`/`scala-2.13`, not the repo's usual `scala-2.12-`/`scala-2.13+`: `scala-2.13+` is also compiled on
  Scala 3, where macros has no scala-reflect. Nothing has to be copied for Scala 3 (its macro is elsewhere), which was the reason for
  the convention. A line in CLAUDE.md says so (B9-R4).
- `UImplicits`: both searches (`Schema[T]` and `Predef[T]`) go through it. `isOwnerSelfRef` stays as the safety net for 2.12 and old 2.13.
- The `Json.auto.derived` re-search (a typechecked `{ val derived = (); implicitly[Predef[T]] }`) can't use the ignoring API: the
  ignored set only applies to the top-level candidate, so nested searches expand `auto.derived` again (prototype: scalac
  `StackOverflowError` on 2.13.18). It keeps the typecheck, also shadows the enclosing definitions' names
  (`{ val derived = (); val p = (); implicitly[...] }`), and its result goes through the same "keep today's winner" check (shadowing
  is ignoring by another means). Without the shadowing, `import Json.auto._` plus `implicit val p: Predef[X] = Predef(Json.schema[X])`
  still warns (prototype: `Implicit resolves to enclosing value p`). With it, also in X's companion (B9-R2): no warning, `p` derives.

Prototype v3: with the lint turned on as an error, 0 warnings; every JVM test passes on 2.13.18 (and v1 on 2.12.21; 2.12 doesn't
take the new path), no golden file changes. With `isSelfRef` also removed (below), the only test that changes is R4.

Old compilers in practice (B9-R3): sbt refuses a 2.13 `scalaVersion` older than the scala-library on the classpath (SIP-51; our
artifacts pull 2.13.18): "Expected `scalaVersion` to be 2.13.18 or later, but found 2.13.16", unless `allowUnsafeScalaLibUpgrade := true`
(checked with 0.9.0 from Central). So sbt users always compile with 2.13.18+ and get the new path. scala-cli allows 2.13.16: the
prototype's jars built on 2.13.18, used from `scala-cli --scala 2.13.16 -Xlint:implicit-recursion`, compile, warn as today, give the
right output (the fallback works, no crash); on `--scala 2.13.18`, no warning.

Pros: no user change; same schemas as today on every compiler (and as Scala 3); scalac does the search, the macro only replays its
ranking between the found implicit and the enclosing one.
Cons: the ranking replay is our code (about 60 lines mirroring `improves` / `isStrictlyMoreSpecific` / `isProperSubClassOrObject`
and the lexical-vs-implicit-scope split), so it can be wrong in corners: an implicit imported *from* a companion of a part of `pt`
counts as implicit scope (declined), and the `-Xsource:3` variance flip in `isAsSpecificValueType` isn't replayed (`Schema` and
`Predef` are covariant, so it doesn't matter for them). Any miss there declines (derives), i.e. it falls back to the self reference's
outcome. Only on 2.13.17+ compilers; 2.13.0–2.13.16 keep today's warning (documented, with the `-Wconf` filter). Two small
version-specific files.

### B. Don't search when an enclosing implicit already provides the type — LOST

Before searching `Schema[T]` (or `Predef[T]`), look at the enclosing owners: if one is an implicit val/def whose result type conforms to
`Schema[T]`, skip the search (go on to `Predef[T]`), as if it had been found and declined. No new API: works on every 2.13 compiler.

Cons: if a more specific implicit is also in scope (P5, P6: an imported or sibling `Schema.object[X]`), today's search returns that
one, and B would skip it: a silent output change. Fixing that means searching anyway, which is A. Lost on correctness; it would
only matter for 2.13.0–2.13.16 compilers, which sbt doesn't allow with our artifacts (see A).

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
nothing to report, and the macro keeps the found implicit only where it would have won against that definition, so the output is
today's; older compilers keep today's search. The source-text heuristic goes, which fixes the README known issue on every Scala 2
version (the only output change).

## Implementation plan

1. `macros/src/main/scala-2.13/com/github/andyglow/jsonschema/ImplicitSearch.scala` and `.../scala-2.12/...` as in A.
2. `macros/src/main/scala-2/.../UImplicits.scala`: `selfSymbols` (enclosing owners + getters + module symbols); `infer(pt)` =
   `inferIgnoring(c)(pt, selfSymbols)` → `unlessSelfWins(pt, y)`, or the plain `c.inferImplicitValue(pt)` on `None`, for the `Schema`
   and the `Predef` search; `unlessSelfWins` with `selfCandidates`, `isLexical`, `beats` as described in A; `selfNames` shadowed in
   the `auto.derived` re-search, whose result also goes through `unlessSelfWins`; remove `isSelfRef` (and `LF`/`CR`, the
   `NoSourceFile` import); comments. `rawInfo.isComplete` needs a cast to `scala.reflect.internal.Symbols#Symbol` (scala-reflect,
   already a dependency; reading the info of an incomplete symbol would be a cyclic reference). The other `c.inferImplicitValue`
   calls (`Flag`, `ToValue`, `KeyPattern`) search types nobody defines with `Json.schema`: unchanged.
3. `project/CompilerOptions.scala` (already in the repro commit): 2.13 gets `-Xlint:implicit-recursion` and
   `-Wconf:cat=w-flag-self-implicit:e`, so a regression fails the 2.13 build (main and test code of every module).
4. Tests (shared `api/src/test/scala`, run on 2.12, 2.13, 3):
   - `SelfImplicitLintSpec` (repro commit): the README pattern with `toDefinition`, a comment inside the type, a companion implicit,
     a generic implicit def. Add: `import Json.auto._` + a self `Predef` (in an object and in the companion); a two-line self
     `Schema` next to a `Predef` of the type (→ the Predef, on all versions); r-b9's P1, P2, P3, P5, P6 with one expectation for
     2.12, 2.13 and 3 (derive, derive, derive, `def(custom)`, `def(custom)`); P3 also with the `Schema.object` in the base trait
     (more specific type vs. subclass: a tie today → derive).
   - `EntryPointsCharSpec` R4: one expectation for both versions (adjacent = separated); drop the `TriageA` tag and the
     "KEEP THESE TWO LINES ADJACENT" comment's Scala 2 part (keep the models: they're the regression test).
   - Golden files: no change (prototype). golden-legacy: no change.
5. Manual check, recorded in the report (done once with the v1 prototype, repeat on the final code): scala-cli on Scala 2.13.16
   with the jars built on 2.13.18 (the fallback: compiles, warns as today, no crash). sbt doesn't allow that combination (see A).
6. CLAUDE.md Conventions: one line that `macros/src/main/scala-2.12` / `scala-2.13` are fine (Scala 3 compiles no Scala 2 macro code).
7. README "Known issues": remove the "line just before" bullet; add one line: on Scala 2.13.0–2.13.16, `-Xlint` still reports
   `implicit val s: Schema[X] = Json.schema[X]` (the compiler can't leave the val out of the search); upgrade to 2.13.17+ or add
   `-Wconf:cat=w-flag-self-implicit:s`. Paradox: `scala3.md` points to the README for Scala 2 issues; check whether any page repeats
   the known issue (none found: `grep` for "previous line" / "line just before"). `makeDocs` gate if a paradox page changes.

## Compatibility

- Source/binary: no public API change (`ImplicitSearch` and `UImplicits` are `private[jsonschema]` macro internals). Patch (0.9.1).
- Compiler support: unchanged. 2.13.0–2.13.16 compilers take the plain search (today's behavior minus `isSelfRef`).
- Output from A: none. The found implicit is used only where today's search would have picked it (P1–P6 above: the same on
  2.12, every 2.13 and Scala 3, pinned in `SelfImplicitLintSpec`).
- Output from removing `isSelfRef`, Scala 2 only (Scala 3 already does this), Neutral and every profile including Legacy:
  - an implicit `Schema[X]` declared on the line just before another `Json.schema` call that uses X is now a definition + `$ref`
    (was: X inlined, the implicit ignored);
  - `implicit val s: Schema[X] = Json.schema[X]` on one or two lines, with an implicit `Predef[X]` in scope, now uses the Predef
    (was: derived X, Predef ignored; three or more lines already used it).
  Both are the README's documented behavior and Scala 3's. No golden or golden-legacy file changes.
- Build: the 2.13 build reports `-Xlint:implicit-recursion` as an error (our build only; not published).

## Review log
B9-R1 (must) the bare ignoring search changes output on 2.13.17+ only (P1–P3) → accepted: A now keeps the found implicit only if it
would have beaten the enclosing one in today's search (lexical scope + scalac's specificity rule, replayed), else declines as today
(section A). Prototype v3 gives master's result for P1–P6 on 2.13.18. P1, P2, P3, P5, P6 (+ a P3 variant) go into
`SelfImplicitLintSpec` with one expectation for all versions; "no output change" / "same as Scala 3" claims corrected.
B9-R2 (nit) companion self Predef + `import Json.auto._` → tested with v3: no warning, derives. Noted in A, and added to the spec.
B9-R3 (nit) old-compiler fallback → verified with scala-cli 2.13.16 (warns as today, correct output, no crash); sbt refuses
`scalaVersion` < 2.13.18 with our artifacts (SIP-51) unless `allowUnsafeScalaLibUpgrade`. The catch is `IncompatibleClassChangeError`,
remembered in the object (per classloader). Noted in A.
B9-R4 (nit) exact-binary dirs → accepted: CLAUDE.md line (plan step 6).
