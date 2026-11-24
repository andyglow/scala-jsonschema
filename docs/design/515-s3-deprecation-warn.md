# Scala 3: deprecation warnings from the derivation can't be silenced  (issue #515, item B21)
Status: draft

## Problem

On Scala 3, deriving a schema for a model that involves a `@deprecated` class makes the macro expansion itself emit
deprecation warnings. A `@nowarn` at the call site doesn't silence them, and some are reported in this library's
own source file, which users don't have:

```scala
@deprecated("use New", "1.0") case class Old(a: Int)
@deprecated("x", "1.0") case class OldVC(v: String) extends AnyVal
@nowarn("cat=deprecation") case class Holder(o: Old, v: OldVC)
sealed trait Fam
@deprecated("x", "1.0") case class FamA(a: Int) extends Fam
case class FamB(b: String) extends Fam
@deprecated("x", "1.0") sealed trait OldFam
@deprecated("x", "1.0") case class OldA(a: Int) extends OldFam

@nowarn("cat=deprecation") def s2 = Json.schema[Old]
@nowarn("cat=deprecation") def s3 = Json.schema[Holder]
@nowarn("cat=deprecation") def s4 = Json.schema[OldFam]
@nowarn("cat=deprecation") def s5 = Json.schema[Fam]
```

The warnings below are from 0.9.0, compiled with `-deprecation`, one call site per compilation. Scala 3.3.9 and
3.10.1 print the same:

| call site | Scala 3 warnings | 2.12 / 2.13 |
|---|---|---|
| `s2` `Json.schema[Old]` | `CallSites.scala:9:49 class Old is deprecated`, "inlined from Products.scala:69" | none |
| `s3` `Json.schema[Holder]` | `Products.scala:80:49 class Old`, `Products.scala:69:36 class Old`, `ValueClasses.scala:24:32 class OldVC` | none |
| `s4` `Json.schema[OldFam]` | call site, "inlined from FamilyEmit.scala:88" (`OldFam`); `Products.scala:69:36 class OldA` | none |
| `s5` `Json.schema[Fam]` | `Products.scala:69:36 class FamA` | call site, suppressible |
| `Json.schema[OldVC]` | call site, "inlined from ValueClasses.scala:24" | none |
| `(Json.schema[Old]: @nowarn(...))` | call site, "inlined from Products.scala:69" | none |
| `Json.sig[Old]` | call site, "inlined from Derivation.scala:86" | none |
| `@nowarn @deprecated case class Der(a: Int) derives json.Schema` | call site, "inlined from Nodes.scala:114"; `Products.scala:69:36 class Der` | n/a |

Every one of them survives the `@nowarn`. Without `@nowarn`, Scala 3 additionally warns about the `[Old]` the user
wrote (`CallSites.scala:8:23`). That warning is legitimate: it's the user's own code, and `@nowarn` silences it.

What doesn't warn: a deprecated case class with defaults (default getters), a deprecated case object in a family, a
deprecated Scala 3 `enum` case, and a `@deprecated` field. A deprecated implicit `Schema` that the derivation picks up
for a field warns at the call site on both Scala 2 and 3, and `@nowarn` silences it on both.

**Cause** (from the 3.3.7 and 3.10.1 compiler sources):
1. The quotes splice the derived types as type arguments: `'{ Schema.`object`.fromList[t]($fs) }` (Products.scala:69),
   `Field[ft](...)` (:80), `` `value-class`[o, i] `` (ValueClasses.scala:24), `` `family`[t] `` (FamilyEmit.scala:88),
   `` `def`.adapt[t] `` (Nodes.scala:114), `TypeSignature[T]` (Derivation.scala:86). The type tree created for a spliced
   `t` gets the position of the `t` inside the quote: this library's file, a source-derived span.
2. `CrossVersionChecks.transformTypeTree` runs on the expanded code, checks every type tree whose span is source-derived,
   and reports a deprecated symbol at the tree's position.
3. `@nowarn` is matched by position: the suppressions of a file are looked up by the warning position's *source*
   (`Run.suppressions`, `mySuppressions.get(dia.pos.source)`), and the span must lie inside the annotated range. A
   position in `Products.scala` never matches a `@nowarn` in the user's file. How the warning is *displayed* (call site
   plus "inlined from", or the library file) depends on the inline trace and doesn't matter for suppression.
   3.10.1 also consults the inline position stack in `Suppression.matches`, but only inside the same source's suppression
   list, so it doesn't help here.
4. Scala 2's deprecation check doesn't look at the type trees of a macro expansion, so it's silent.

No upstream issue matches this case (scala/scala3 search, 2026-11-23). The nearest is play-json#1488, which has the
same "can't silence warnings from macro code" symptom.

## Why it is a problem

- A project with `-Werror` / `-Xfatal-warnings` that keeps deprecated model classes (a common way to phase out an API
  version) can't compile on Scala 3 without disabling the warning globally. On Scala 2 the same code is clean.
- The warnings name a file and line inside this library (`core/src/main/scala-3/.../Products.scala:80`) that users
  can't open, and a single call site can produce several of them.
- The only workaround today is a global filter: `-Wconf:cat=deprecation&src=.*jsonschema/derivation/.*:s`. It silences
  exactly these warnings and keeps the user's own ones.
- Our own test models hit it as well: deprecated test models on Scala 3 either need a deprecated enclosing owner or
  produce warnings, which breaks the warning-free build rule (c-e8, neighbour finding 2).

## Options

### A. Re-create the expansion's type trees at the call site  — WON

Once the expansion is built, a `TreeMap` replaces every type tree in it with `Inferred(tree.tpe)`. `Inferred` is
positioned at the macro expansion (`withDefaultPos` → `Position.ofMacroExpansion`), which is the user's call site. The
deprecation check still runs, but the warning's source is now the user's file, so a `@nowarn` (or `-Wconf:src=...`)
at the call site matches. The types themselves are unchanged, and so is everything else in the tree.

```scala
object CallSitePositions {          // core/src/main/scala-3/.../derivation, INTERNAL API
  def apply[T: Type](e: Expr[T])(using q: Quotes): Expr[T] = {
    import q.reflect.*
    val map = new TreeMap {
      override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = Inferred(tree.tpe)
      // the stock TreeMap doesn't descend into a Bind's pattern (`case d: Schema.def[t] =>`)
      override def transformTree(tree: Tree)(owner: Symbol): Tree = tree match {
        case b: Bind => Bind.copy(b)(b.name, transformTree(b.pattern)(owner))
        case _       => super.transformTree(tree)(owner)
      }
    }
    map.transformTerm(e.asTerm)(Symbol.spliceOwner).asExprOf[T]
  }
}
```

It is applied once per expansion, at the entry points (`SchemaMacro.*Impl`, `DerivedSchemaMacros`, `RefinedMacro`).

**Prototype** (uncommitted, saved in `notes/scratch/c-b21/prototype/`). On 3.3.9, with the macro built by 3.3.9 and
the consumer compiled by 3.3.9 or by 3.10.1:
- with `@nowarn` on the call site, every row of the table above is clean, under `-Xcheck-macros` too: `s2`, `s3`,
  `s4`, `s5`, the value class, the expression-level `@nowarn`, `objectSchema`, `sig`, `auto`, `derives` (also unsuppressed);
- without `@nowarn` they still warn, as they should. `s3` gives one warning instead of three: they all share the
  call-site span now, and the compiler's `UniqueMessagePositions` reporter keeps the first;
- `sbt "++3.3.9" clean test`: 1094 tests passed, 0 failed, with no warnings beyond the repro's own unsuppressed calls. A
  debug traverser confirmed that no type tree positioned in the library's sources is left in any expansion of the api
  test suite.

Pros: one small internal helper, no change to the IR or to any emitter, and it covers type trees added by future
emitters automatically. It works on 3.3 LTS and on current Scala 3. `@nowarn` and `-Wconf:src=<user file>` behave the
way they do on any other code. Schemas and runtime behaviour are unchanged.

Cons:
- One extra tree traversal per expansion. It is linear in the expansion's size. It hasn't been measured yet; step 6
  of the plan measures it.
- Without `@nowarn`, a warning from a nested part (`s3`, `s5`) is still *displayed* at the library's file
  (`Products.scala:80:27`): the display follows the quote's inline trace, which a `TreeMap` can't move. Suppression
  follows the tree's own position, which is fixed. This is cosmetic, and it shows up only when the user hasn't silenced
  the warning.
- Users who adopted the `-Wconf:...&src=.*jsonschema/derivation/.*:s` workaround see these warnings again, now at
  their own call sites (verified). See Compatibility.

Variant A2, rewriting only the type trees whose type mentions a `@deprecated` symbol, LOST. It needs a hand-written type
walker (applied types, `&`/`|`, refinements, lambdas, annotated types, prefixes) that can miss a case, and rewriting a
harmless tree costs as much as checking it.

### B. Give the expansion a deprecated owner  — LOST

The compiler skips a deprecation warning when any enclosing owner is deprecated (`skipWarning` /
`skipDeprecation`). The expansion could live in one:
`(new Thunk[Schema[T]] { @deprecated("", "") def get = <expansion> }: Thunk[Schema[T]]).get`.
`.get` resolves to the abstract, non-deprecated `Thunk.get`, so the call itself doesn't warn.

- Pros: total silence, even without `@nowarn` (Scala 2 parity for the "none" rows).
- Cons:
  - one anonymous class per call site (a class file for each `Json.schema` / `derives`);
  - it also silences warnings the user should see, e.g. a deprecated implicit `Schema` the derivation picks up (Scala 2
    warns there);
  - `Thunk[Schema[T]]` itself mentions `T`, so it needs Option A's rewrite anyway.
- The simpler form, a local `@deprecated def w = <expansion>; w`, was prototyped and fails: the reference `w` warns
  "method w is deprecated", at the library position again.
- More code and more bytecode for silence that the user can get with one `@nowarn` under A.

### C. Document the `-Wconf` workaround only  — LOST

Add `-Wconf:cat=deprecation&src=.*jsonschema/derivation/.*:s` to README / paradox and change no code.

- Pros: zero risk.
- Cons: the filter is global, the warnings still name a file users don't have, and users can't silence one call site.
  Our own deprecated test models would still need the workaround in the build.
- Kept as the documented workaround for 0.8.x / 0.9.0 (see Compatibility).

### D. Wait for the compiler  — LOST (complementary)

Matching `@nowarn` against the outermost inline position would fix this in the compiler for every macro library.
3.10.1 moved partly in that direction (`inlinePosStack` in `Suppression.matches`) and still fails here. Even a fix
would reach 3.3 LTS users late or never. Filing an upstream issue with this repro is the owner's call, and it's
compatible with A.

## Decision

Option A. After a derivation is built, every type tree in it is re-created as `Inferred(tpe)` at the macro call site,
so the compiler reports the derived types' deprecations where the user called the macro, and `@nowarn` /
`-Wconf:src=...` there work. Schemas don't change.

## Implementation plan

1. **`core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/CallSitePositions.scala`** (new, INTERNAL API):
   the helper shown in Option A, with a scaladoc that explains why (link #515). It includes the `Bind` override,
   because the stock `TreeMap` doesn't rewrite a `Bind`'s pattern. Today the only such pattern is `Schema.def[?]` at
   Nodes.scala:95, which never names a user type, but a future pattern might. Leave out the prototype's
   `b21.debug` traverser.
2. **Apply it once per expansion**, at the outermost `${ }` targets:
   - `SchemaMacro.schemaImpl`, `objectSchemaImpl`, `derivedImpl`, `autoImpl`, `sigImpl`: wrap the result
     (`CallSitePositions(Derivation.derive[T](Mode.Schema))`, ...).
   - `derivedSchemaImpl` stays unwrapped: its only caller,
     `modules/derived/src/main/scala-3/json/schema/derived/DerivedSchemaPlatform.scala` `DerivedSchemaMacros.deriveDerivedSchema`,
     wraps its own outer quote, `'{ DerivedSchema[T](...) }`, which contains the derivation.
   - `modules/refined/src/main/scala-3/.../refined/RefinedMacro.scala` `predef` and `schema`: wrap. A deprecated base
     type is unusual, but the cost is nil, and consistency means future emitters are covered.
     `RefinedMacro.jsonSchema` emits `json.Json.schema[x]`, a nested macro call that gets its own wrapper.
   - Don't wrap in `Derivation.run` / `deriveObject`: the entry points are the single place, and nothing would be
     rewritten twice.
3. **Tests** (`api/src/test/scala/com/github/andyglow/jsonschema/DeprecatedModelsSpec.scala`, shared by all three
   Scala versions):
   - models: a deprecated case class, a case class with default values, a value class, a sealed family, a deprecated
     member of a non-deprecated family, a case class whose field type is deprecated, and a deprecated case object in a
     family. Mark each definition that needs it `@nowarn("cat=deprecation")`: on Scala 2 the model definitions
     themselves warn.
   - every call site carries `@nowarn("cat=deprecation")`: `Json.schema`, `Json.objectSchema`, `Json.sig`,
     `Json.auto.derived` (via `import Json.auto.*`), and a field of a non-deprecated class.
   - the assertions check the schemas, so the spec doubles as a characterization test: deprecation has no effect on
     the output (E8/#481 adds the `deprecated` keyword; if E8 merges first, these assertions show it).
   - Scala 3 only (`api/src/test/scala-3/.../Scala3DeprecatedModelsSpec.scala`): `@deprecated case class D(a: Int) derives json.Schema`
     with and without `@nowarn`, and a deprecated `enum` case.
   - `modules/derived` Scala 3 test: one `DerivedSchema[Old]` under `@nowarn`.
   - **Make a regression fail the build:** in `build.sbt:100` (the Scala 3 `Test / scalacOptions` line, next to
     `-Yread-docs`, `-Xcheck-macros`), add `-Wconf:cat=deprecation:e`. An unsuppressed deprecation in any Scala 3 test
     then becomes a compile error, and a `@nowarn` still wins over `-Wconf`, because suppression is checked first. The
     tests are warning-free today, so nothing else changes. Check with the gates. If the reviewer or lead prefers
     not to touch the global test flags, scope it to `api` and `derived` instead.
   - Verify the new tests catch the bug: on unfixed master they must fail to compile on 3.3.9, with the
     `Products.scala:69/80`, `ValueClasses.scala:24` and `FamilyEmit.scala:88` errors.
4. **No golden-file changes** and no change to `golden-legacy/`: the output doesn't change.
5. **Docs** (README "Scala 3 notes" and `docs/src/main/paradox/scala3.md`, same words). Add one bullet: "Deriving a
   schema for a `@deprecated` type reports the deprecation at the `Json.schema` / `derives` call site, where
   `@nowarn("cat=deprecation")` silences it (0.8.0–0.9.0: the warnings pointed into the library's sources and needed
   `-Wconf:cat=deprecation&src=.*jsonschema/derivation/.*:s`)." No CHANGELOG edit (the lead writes it).
6. **Gates** (protocol step 8): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core changes);
   `scalafmtCheckAll` + `rootJS/scalafmtCheckAll` on each; `scalafmtSbtCheck` (build.sbt changes); `makeDocs`. Also:
   - `sbt "++3.3.9" clean api/Test/compile` timed before and after the change, to put a number on the traversal cost.
     More than ~5% slower means: report it, and then consider A2.
   - one consumer compile on the newest Scala 3, through scala-cli against `publishLocal`, as done for the prototype
     with 3.10.1.

Scala 2: no change; the Scala 2 macro has no such warnings. JS: the change is compile-time only, and the macro
runs the same on JVM and JS; the shared spec runs on JS as well.

Risks:
- a type tree that `Inferred` can't stand in for, in a position where the compiler expects a specific tree shape.
  The prototype passed `-Xcheck-macros` on the whole api suite (sums, recursion, enums, value classes, `derives`,
  generic `derives`, Options with defaults), and so does any new emitter as long as the suite covers it;
- error messages from later phases that point at a type tree of the expansion now point at the call site. That's
  arguably better, and no test asserts on such positions.

## Compatibility

- Source and binary: no public API change. A new internal object lives in the Scala 3 `core` artifact. The inline
  definitions (`Json.schema`, `Schema.derived`, ...) keep their bodies, and only what the macro implementation
  returns changes, so code compiled against 0.9.0 keeps working and picks the fix up at its next compile.
- Output: no schema changes, under any profile; Legacy is untouched.
- Diagnostics: on Scala 3, the derivation's deprecation warnings move from the library's source positions to the
  user's call site, and `@nowarn` / `-Wconf:src=<user file>` now silence them. Users who adopted the
  `-Wconf:...&src=.*jsonschema/derivation/.*:s` workaround (verified) see the warnings again, at their own call
  sites, and should switch to `@nowarn` at the call site or to a `-Wconf` `origin=` filter. `cat=deprecation&origin=pkg.Old:s`
  works before and after the fix (verified on 0.9.0 and on the prototype).
- PVP: patch (0.9.x).

## Review log
