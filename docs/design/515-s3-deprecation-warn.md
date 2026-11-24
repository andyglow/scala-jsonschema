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
4. Scala 2 doesn't warn about the type arguments of an expansion: `Schema.`object`[Old](...)` in `s2`'s expansion is
   silent, and every position in a Scala 2 expansion is the call site anyway. For `s5`, its expansion has
   `Some(classOf[b21.FamA])` for each family member (seen with `-Xmacro-settings:print-jsonschema-code`). That
   `classOf` term is most likely what Scala 2 flags, at the call site, where `@nowarn` works.

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

### A. Re-create the expansion's type trees at the call site, and drop the quotes' position markers  — WON

Once the expansion is built, one `TreeMap` pass makes two changes:

1. **Type trees.** Every type tree is replaced with `Inferred(tree.tpe)`. `Inferred` is positioned at the macro
   expansion (`withDefaultPos` → `Position.ofMacroExpansion`), which is the user's call site. The deprecation check
   still runs, but the warning's source is now the user's file, so a `@nowarn` (or `-Wconf:src=...`) at the call site
   matches. The types themselves don't change.
2. **Position markers.** The quotes' `Inlined` position markers are unwrapped (review B21-R1). Without this step, a
   warning from a nested part of the expansion is still *displayed* at the library's file (`s3`:
   `Products.scala:80:27`, `s5`: `Products.scala:69:11`), with no call-site line, even though `@nowarn` silences it.
   The display follows the inline trace that these markers build, not the tree's own position. The rewrite meets two
   kinds of markers (logged over the whole api suite):
   - `Inlined(Some(Ident(<the quote's top-level class>)), Nil, body)`, one per quote. The call is the macro source's
     class or module: `Products`, `Nodes`, `FamilyEmit`, `Options`, `Implicits`, `Derivation$`, `DerivedSchemaMacros$`.
     The suite has 100 of them.
   - `Inlined(None, Nil, body)`, the "from the outer scope" wrapper around each spliced hole. The suite has 595.

   Both are replaced with `body`. An `Inlined` whose call is a *method* is another inline method's expansion (say, an
   inline given found by implicit search), not ours, and is left exactly as it is, including the type trees inside it.
   The suite has no such node, so this is a guard (see Risks).

   Every tree carries its own source file, so unwrapping a marker changes no position. The 3.3 and 3.10 JVM
   backends take line numbers from `tree.sourcePos` (the tree's own source), so the bytecode line numbers don't
   change either. Only the inline trace of a diagnostic changes.

```scala
object CallSitePositions {          // core/src/main/scala-3/.../derivation, INTERNAL API
  def apply[T: Type](e: Expr[T])(using q: Quotes): Expr[T] = {
    import q.reflect.*
    // a quote's position marker (call = the quote's top-level class) or a spliced hole's (no call)
    def isQuoteMarker(call: Option[Tree]): Boolean = call.forall(_.symbol.isClassDef)
    val map = new TreeMap {
      override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = Inferred(tree.tpe)
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case Inlined(call, Nil, body) if isQuoteMarker(call) => transformTerm(body)(owner)
        case Inlined(Some(_), _, _)                          => tree // another inline method's expansion: not ours
        case _                                               => super.transformTerm(tree)(owner)
      }
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

**Prototype** (uncommitted, saved in `notes/scratch/c-b21/prototype/`: `CallSitePositions-r1.scala`, `tracked.patch`;
the experiment switches `b21.unwrap` / `b21.unwrapNone` are system properties). Macro built by 3.3.9, consumer compiled
by 3.3.9 or by 3.10.1, `-Xcheck-macros` on:
- with `@nowarn` on the call site, every row of the table above is clean: `s2`, `s3`, `s4`, `s5`, the value class,
  the expression-level `@nowarn`, `objectSchema`, `sig`, `auto`, `derives`;
- without `@nowarn` they still warn, as they should, and every one is shown **at the call site with no inline trace**:
  `s3` → `CallSites.scala:10:22 class Old is deprecated`, `s5` → `CallSites.scala:12:22 class FamA is deprecated`. One
  warning per call site: they all share the call-site span, and the compiler's `UniqueMessagePositions` reporter shows
  only the first (`s3`'s `OldVC` warning is hidden behind `Old`'s);
- `sbt "++3.3.9" clean test` with both changes: 1094 tests passed, 0 failed, with no warnings beyond the repro's own
  unsuppressed calls. In the type-trees-only run, a debug traverser confirmed that no type tree positioned in the
  library's sources is left in any expansion of the suite.

Pros: one small internal helper, no change to the IR or to any emitter, and it covers trees added by future emitters
automatically. It works on 3.3 LTS and on current Scala 3. A warning is shown where the user can act on it, and
`@nowarn` / `-Wconf:src=<user file>` behave the way they do on any other code. Schemas and runtime behaviour are
unchanged.

Cons:
- One extra tree traversal per expansion. It is linear in the expansion's size. It hasn't been measured yet; step 6
  of the plan measures it.
- One warning per call site (review B21-R4). Several deprecated types reached from one `Json.schema` collapse into the
  first; without `@nowarn` the user meets them one compile at a time. One `@nowarn` silences them all.
- A diagnostic a later phase might report on a *term* of the generated code (none does today: the suite is
  warning-free) loses its "inlined from" trace. It is shown at the library's position on its own.
- Users who adopted the `-Wconf:...&src=.*jsonschema/derivation/.*:s` workaround see these warnings again, now at
  their own call sites (verified). See Compatibility.

Variant A1, type trees only without the marker unwrap (design 57a681f), LOST: `@nowarn` works, but an unsuppressed
nested warning is shown in a file the user doesn't have, so they can't tell which call site to annotate (review
B21-R1).

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
would reach 3.3 LTS users late or never. Filing an upstream issue is the owner's call, and it's compatible with A.
A ready repro, a quote `'{ List.empty[T] }` and a `@nowarn` call site, is in `notes/scratch/c-b21/upstream/`
(`Macro.scala`, `Use.scala`, commands in the header). It warns, unsuppressed, on 3.3.7 and 3.10.1.

## Decision

Option A. After a derivation is built, every type tree in it is re-created as `Inferred(tpe)` at the macro call site,
and the quotes' `Inlined` position markers are unwrapped. The compiler then reports the derived types' deprecations
at the user's call site, shown there too, and `@nowarn` / `-Wconf:src=...` there work. Schemas don't change.

## Implementation plan

1. **`core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/CallSitePositions.scala`** (new, INTERNAL API):
   the helper shown in Option A, with a scaladoc that explains why (link #515). Both parts go in: the type-tree
   rewrite and the marker unwrap, including the guard that leaves a method-call `Inlined` alone. It also includes the
   `Bind` override, because the stock `TreeMap` doesn't rewrite a `Bind`'s pattern. Today the only such pattern is
   `Schema.def[?]` at Nodes.scala:95, which never names a user type, but a future pattern might. Leave out the
   prototype's `b21.*` system-property switches and its debug traverser.
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
   - Scala 3 only (`api/src/test/scala-3/.../Scala3DeprecatedModelsSpec.scala`), every case under `@nowarn`:
     `@nowarn @deprecated case class D(a: Int) derives json.Schema`; `@nowarn case class H(o: Old) derives json.Schema`
     (a deprecated field type behind `derives`; 0.9.0 warns at `Products.scala:80` here); and a deprecated `enum` case.
   - **No test without `@nowarn`** (review B21-R2). The prototype shows that `derives` on a deprecated case class is
     silent even without `@nowarn` (0.9.0: two warnings, one of them at `Products.scala:69`). I haven't confirmed why:
     probably the compiler's exemption for synthetic members of a deprecated case class's companion
     (`siteIsSyntheticCaseClassMember`). That isn't our contract. Our contract is "suppressible at the call site", and a
     test of the unsuppressed case would break on a compiler change that is out of our hands. The doc and the PR record
     the observation; no test asserts it.
   - **Guard test** (Scala 3 only, same spec; review B21-R8): a model whose field type has an `inline given Schema` /
     `Predef` that implicit search finds during derivation. This puts a method-call `Inlined` into the expansion, so
     the "leave it alone" branch of `CallSitePositions` runs. Assert the schema, so the inline given's output is
     unchanged. Confirm once, with a temporary `report.info`, that the branch is hit, and note in the PR what the
     rewrite saw.
   - `modules/derived` Scala 3 test: one `DerivedSchema[Old]` under `@nowarn`. `modules/refined`: one refined field
     inside a model with a deprecated sibling field type, under `@nowarn`.
   - **Make a regression fail the build** (scoped per review B21-R3): add `-Wconf:cat=deprecation:e` to the Scala 3
     `Test / scalacOptions` of **api, derived and refined** only, not to `commonSettings` (`build.sbt:100`). Other
     libraries' macros (play-json#1488 has the same symptom) could otherwise turn a Scala Steward bump into an error
     nobody can `@nowarn`. An unsuppressed deprecation in those modules' Scala 3 tests becomes a compile error, and a
     `@nowarn` still wins over `-Wconf`, because suppression is checked first. Their tests are warning-free today.
     Check with the gates.
   - Verify the new tests catch the bug: on unfixed master they must fail to compile on 3.3.9, with the
     `Products.scala:69/80`, `ValueClasses.scala:24` and `FamilyEmit.scala:88` errors.
   - **E8 / #485 interaction** (review B21-R5): whichever of B21 and E8 merges second rebases and re-runs the Scala 3
     tests under the new `:e`. If E8 lands first, this spec's assertions follow E8's output (`deprecated: true` on
     2019-09+, stripped under Legacy). E8's Scala 3 deprecated test models use a deprecated enclosing owner to stay
     quiet. After B21 that can become a plain `@nowarn`; that's optional and E8's call.
4. **No golden-file changes** and no change to `golden-legacy/`: the output doesn't change.
5. **Docs** (README "Scala 3 notes" and `docs/src/main/paradox/scala3.md`, same words). Add one bullet: "Deriving a
   schema for a `@deprecated` type reports the deprecation at the `Json.schema` / `derives` call site, where
   `@nowarn("cat=deprecation")` silences it. The compiler shows one such warning per call site, even when several
   deprecated types are involved (0.8.0–0.9.0: the warnings pointed into the library's sources and needed
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
  arguably better, and no test asserts on such positions;
- the marker unwrap. Other code could depend on the quotes' `Inlined` markers; the prototype shows that none does.
  Positions are per tree (each tree keeps its source file); the 3.3 and 3.10 backends compute line numbers from the
  tree's own source; `-Xcheck-macros` passed on the whole suite; and 3.10.1 as a consumer gives the same diagnostics.
  What changes is the inline trace of a diagnostic on a term of our generated code (see Cons);
- the method-call `Inlined` guard: the current suite has no such node. Plan step 3 adds a test that exercises it. If
  such an expansion carries type trees from our own quotes (it would be our macro nested in theirs), its own entry
  point has already rewritten them;
- not ours, unchanged: `@deprecated sealed trait F derives json.Schema` with a deprecated member `F1` gets one more
  warning from the compiler at the `derives` clause ("class F1 is deprecated", no inline trace). 0.9.0 has it too, and
  the fix keeps it. It most likely comes from the compiler's `Mirror` synthesis for `derives`.

## Compatibility

- Source and binary: no public API change. A new internal object lives in the Scala 3 `core` artifact. The inline
  definitions (`Json.schema`, `Schema.derived`, ...) keep their bodies, and only what the macro implementation
  returns changes, so code compiled against 0.9.0 keeps working and picks the fix up at its next compile.
- Output: no schema changes, under any profile; Legacy is untouched.
- Diagnostics: on Scala 3, the derivation's deprecation warnings move from the library's source positions to the
  user's call site and are shown there, one per call site, and `@nowarn` / `-Wconf:src=<user file>` now silence them. Users who adopted the
  `-Wconf:...&src=.*jsonschema/derivation/.*:s` workaround (verified) see the warnings again, at their own call
  sites, and should switch to `@nowarn` at the call site or to a `-Wconf` `origin=` filter. `cat=deprecation&origin=pkg.Old:s`
  works before and after the fix (verified on 0.9.0 and on the prototype).
- PVP: patch (0.9.x).

## Review log

Round 1 (r-b21, on 57a681f):
- B21-R1 (must) fix the display too → **accepted**. Option A now also unwraps the quotes' `Inlined` markers. The
  markers are `Inlined(Some(Ident(<the quote's top-level class>)), Nil, _)` (100 in the suite) plus the hole wrappers
  `Inlined(None, Nil, _)` (595). Both are unwrapped. An `Inlined` with a method call is left alone (a guard; there
  are none in the suite). Re-run without `@nowarn`: `s3` → `CallSites.scala:10:22`, `s5` → `CallSites.scala:12:22`,
  no trace, on 3.3.9 and 3.10.1. With `@nowarn`, every case is clean. `clean test` on 3.3.9 passes (1094, `-Xcheck-macros`).
  The backends take line numbers from each tree's own source, so unwrapping doesn't change them. Updated: the Option A
  text and code, Cons, Risks, Decision, plan step 1; the type-trees-only version is now variant A1 (LOST).
- B21-R2 (must) unsuppressed `derives` vs `:e` → **accepted**: dropped. Every test carries `@nowarn`. The observation
  that unsuppressed `derives` on a deprecated case class is silent after the fix (cause unconfirmed) is recorded in
  plan step 3, not tested.
- B21-R3 (nit) scope `-Wconf:cat=deprecation:e` → **accepted**: api, derived and refined only, not `commonSettings`.
- B21-R4 (nit) dedup → **accepted**: one warning per call site, in Cons, in the prototype notes and in the docs
  bullet (step 5).
- B21-R5 (nit) E8 interaction → **accepted**: new bullet in step 3.
- B21-R6 (nit) cause bullet 4 vs s5 → **accepted**: Scala 2's `s5` expansion has `classOf[b21.FamA]` per member, most
  likely what it flags; its type arguments don't warn (`s2`).
- B21-R7 (nit) upstream toy → **accepted**: `notes/scratch/c-b21/upstream/` (warns on 3.3.7 and 3.10.1), pointer in
  Option D.

Round 2 (r-b21, on cb61edd):
- B21-R8 (nit) move the inline-given guard test into the test plan → **accepted**: plan step 3 "Guard test"; the Risks
  bullet points there.
