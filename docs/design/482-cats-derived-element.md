# cats collections of a derived element  (issue #482, item E1)
Status: draft

## Problem
The cats instances (`CatsSupport._`) are "Predef constructors": `implicit def nelSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyList[T]]`
(and a higher-priority twin taking `Schema[T]`). Both derivation macros look up an implicit for the type *as a whole* (`Predef[NonEmptyList[P]]`).
When the element `P` is a case class with no implicit of its own, that search fails, and the macro derives `NonEmptyList[P]` from its structure.

```scala
import cats.data._, json.Json, com.github.andyglow.jsonschema.CatsSupport._
case class P(a: Int)
```

| type | 2.12 / 2.13 / 3.3 today (JVM and JS: same) |
|---|---|
| `NonEmptyList[P]`, `OneAnd[List, P]`, `NonEmptyStream[P]` (2.12) | a case class: `{"type":"object","properties":{"head":{..P..},"tail":{"type":"array","items":{..P..}}},"required":["head","tail"]}` |
| `Chain[P]` | a sealed family: `oneOf` of Chain's internals `Empty`, `Singleton{a}`, `Wrap{seq}`, `Append{leftNE,rightNE}` + a definition `cats.data.Chain.NonEmpty[P]` (member order differs 2 vs 3) |
| `NonEmptyVector[P]` | `error: schema for cats.data.NonEmptyVector[P] is not supported,` |
| `NonEmptySet[P]`, `NonEmptyChain[P]`, `NonEmptyMap[K, P]`, `NonEmptyLazyList[P]` | `error: schema for cats.data.NonEmptySetImpl.Type[P] is not supported,` (newtypes: nothing to derive) |

Repro: `modules/cats/src/test/scala/com/github/andyglow/jsonschema/CatsDerivedElementSpec.scala` (on this branch): 5 compile errors on 2.12.21, 2.13.18 and 3.3.9.
With a built-in element (`NonEmptyList[String]`) all of them work. With `implicit val p: Schema[P] = Json.schema[P]` they compile, but the
higher-priority `Schema[T]` variants (`nelSchema(ss)`, ...) embed the user's schema value as is, so `P` is **inlined**, while `List[P]` makes it a
definition + `$ref` (the core rule). Verified by r-e1 on 2.13 and 3 (E1-R1); the lead put that in E1's scope: the rule wins.

Two more cats bugs, added to E1 by the lead (2026-11-23), both in `CatsSupport.scala`, any element:
- `nesSchema` (the `Schema[T]` variant) calls `mkNEx(ss)` without `unique = true`: `NonEmptySet[X]` with an implicit `Schema[X]` has no
  `uniqueItems`, with a `Predef[X]` it has (found by c-e8).
- `Chain[T]` (both variants) goes through `mkNEx`, which always adds `minItems: 1`, but a `Chain` can be empty: the schema rejects `[]`.

## Why it is a problem
A case class element without an implicit is the common case (`List[P]` works that way, so users expect `NonEmptyList[P]` to). The structural
`{head, tail}` / Chain-internals schemas are wrong: circe, play-json, upickle and every other codec write these types as JSON arrays (objects for
`NonEmptyMap`), so a validator rejects every real value. The other types are a compile error with no hint. Reported before as #171 (closed after the
user added an implicit `Schema` for the element, which also turns `P` into a definition, not what they asked for).

The fix can't be in the cats module alone: its instances need *some* implicit for `T`, and only the macro knows how to derive `T`.

## Options

### A. "Predef constructor" retry with marker types, in both macros — WON
When a type `F[A1..An]` has no implicit `Schema`/`Predef`, isn't an `Option`/`Map`/collection (the built-in families keep it), and some of its proper type
arguments have no implicit `Predef`, the macro asks: *would there be a `Predef[F[..]]` if those arguments had a `Predef`?* It searches
`Predef[F[.., Hole1, ..]]`, with each such argument replaced by a marker type `HoleK` from core that has an implicit `Predef` and nothing else:

```scala
// core (shared), INTERNAL API
package com.github.andyglow.jsonschema.internal
sealed trait Hole1; object Hole1 { implicit val predef: Predef[Hole1] = Predef(...) }   // also Hole2..Hole4
```

For `NonEmptyList[P]` the probe finds `CatsSupport.nelSchemaFromPredef[Hole1](Hole1.predef)`. The macro then resolves each replaced argument with the
full rule (derived → inlined, implicit `Schema` → definition + `$ref`, recursion → `$ref`) and emits the constructor applied to it:

Both versions emit **the probe's own tree** with the markers substituted, so they apply the same constructor (E1-R4):

- **Scala 3**: the found tree with `HoleK` substituted by the argument in its type trees (`TypeRepr.substituteTypes`) and each `HoleK.predef` reference
  replaced by `Predef[A](<A's schema>)`; the reflect tree copier re-types the rebuilt `Apply`/`TypeApply` nodes. Then the result must be a `Predef[F[..]]`.
  (A quote-local `given Predef[P]` is not visible to `Implicits.search`, checked in a spike, so Scala 3 can't use a local implicit.)
- **Scala 2**: the found tree, `c.untypecheck`ed, with type trees substituted the same way and each `HoleK.predef` reference replaced by a local
  `e_K`: `{ val e1: Predef[P] = Predef(<P's schema>); <tree> }.schema`. Before the element is resolved, the same block with `= null` is typechecked
  (`c.typecheck(silent = true)`), so the expansion is known to compile; otherwise the type falls through to the structural families as today.
  (Emitting `implicitly[Predef[F[P]]]` under a local implicit instead would let scalac pick a more specific constructor for `P` than the one
  found for `Hole1`, which Scala 3 can't: r-e1's specificity case.)

**The probe tree check** (same rule on both versions, before anything is emitted): a marker may appear only as a type argument of an application or
as a `HoleK.predef` reference. Anything else falls back to structural derivation:
- a macro expansion that mentions a marker (Scala 2: a tree with a `MacroExpansionAttachment`; Scala 3: an `Inlined` whose call or bindings mention
  one): the macro ran for `Hole1`, not for `P` (refined's `Refined[A, Hole1]`, any inline given that inspects `T`);
- a constant that mentions one: `classOf[Hole1]` inside a synthesized `ClassTag[Hole1]` (E1-R4), a `TypeTag`, ... : the evidence was made for the marker.
So a constructor that needs `ClassTag[T]`, `Order[T]`, a bound `T <: Foo`, or that is itself a macro, keeps today's output, on both versions.

Why markers are sound: `HoleK` is a fresh sealed trait whose only instance is its `Predef`, so anything the search found for it is generic in that
argument (parametricity) and stays valid after substituting `P`, except evidence synthesized for the marker itself (constants, macro expansions),
which the probe tree check rejects. A constructor that needs more about the element fails the probe or the check, and the type is derived as today:
never worse than now. Both versions run the same probe and emit its tree: same constructors accepted, same constructor applied (parity).

**Elements with an implicit `Schema` (E1-R1, lead decision 2026-11-23: the core rule wins).** An argument gets a hole when it has no accepted
implicit `Predef`, so an element with only an implicit `Schema` gets one too, and is then resolved by the rule: definition + `$ref`, like `List[P]`.
For that to reach the cats types, cats' `Schema[T]` variants (`nelSchema`, `nevSchema`, `nesSchema`, `necSchema`, `chainSchema`, `nemStrSchema`,
`oneAndSchema`, `nellSchema`) must stop being implicit: today the outer search finds them first and they inline the value. They can't be made to
produce a `$ref` instead: the definition name is computed by the macro at the use site (`Schema.def.adapt(x, signature)`), a runtime `Schema[T]` has
no name, and passing one would need a new implicit `TypeSignature[T]` materializer (new API). With E1 they are redundant, so they stay as plain
`@deprecated` defs (source and binary compatible for explicit calls); not deprecated implicits, which would warn at every user's `Json.schema` call
site through the macro's output. Told the lead (2026-11-23).

**`Json.auto` (E1-R2).** Scala 3: the probe runs under `AutoGuard.searching`, so a lexical-scope `auto.derived[Hole1]` declines and the search goes on
to `Hole1.predef` in implicit scope; the cats types then work with or without the import (today, with it, NEV[P] is still "not supported" on 3).
Scala 2: with the import the outer search already succeeds today (`nelSchemaFromPredef(auto.derived[P])`, accepted because `isAutoDerived` checks only the
root); unchanged. For a type without a constructor (`Box[P]`), the probe uses `Implicit`'s `inferPredef`, which already shadows `auto.derived` when
the search picks it, so the probe never takes `auto.derived[Box[Hole1]]`. Tests on all versions (plan step 4).

**Third-party macro implicits (E1-R3).** refined's `refinedJsonSchema[A, B]` is a macro `Predef` for every `Refined[A, B]`; a refined type whose
macro fails (an empty range) reaches the probe with `B` as a hole, and the probe expands refined's macro for `Refined[Int, Hole1]`. The probe must be
silent: Scala 3 searches without `recordFailure`; Scala 2 runs the probe as `c.typecheck(q"implicitly[$pt]", silent = true)` (macro aborts inside become
a failed typecheck, not a reported error) rather than `c.inferImplicitValue`, if the latter turns out to report them. A success is rejected anyway by
the probe tree check (a macro expansion mentioning a marker). Checked by running refined's error specs (Scala 2 and 3) unchanged: no message may
mention `Hole`; the result goes into this doc.

**Known limitations.** (E1-R5) A map key without a `Predef` (a derived string enum, a Scala 3 `enum`) is a hole too, and `KeyPattern[HoleK]` fails the
probe: `NonEmptyMap[Color, P]` stays "not supported" (`Map[Color, P]` works natively). (E1-R8) The root of `derives` and of `DerivedSchema` is not
probed, like the root implicit search: `DerivedSchema[NonEmptyList[P]]` stays structural `{head, tail}`, as `DerivedSchema[NonEmptyList[Int]]` is today;
`NonEmptyList[P]` as a *field* of a `derives`/`DerivedSchema` root works.

Where it sits: a new structural family *after* `Option`, `Map` and `Iterable`/`Array` and *before* enums, sums, case classes and value classes
(Scala 3 `Resolve.families`, Scala 2 `genTree`). So `List[P]`, `Map[K, P]` keep their nodes and output (core's `listS`/`dictS` are Predef constructors
too, the retry would otherwise take them over), and `NonEmptyList[P]` (a case class) or `Chain[P]` (a sealed class) get it before structural derivation.
It is skipped for the root of `derives`/`DerivedSchema` (the root search is skipped there too).

Sample output: `Json.schema[NonEmptyList[P]]` = `array[P, NonEmptyList](object(a: integer)).withValidation(minItems := 1)`, i.e.
`{"type":"array","items":{"type":"object",...P...},"minItems":1}`; `NonEmptySet[P]` adds `uniqueItems`; `NonEmptyMap[Int, P]` is a dictionary with
`minProperties` and the key pattern. `case class Tree(v: Int, kids: Chain[Tree])` gives `kids: {"type":"array","items":{"$ref":"#/definitions/..Tree"}}`.

Pros: no cats-specific code in the macros, no new user API; every existing and future Predef constructor benefits, the user's own
(`implicit def boxP[T](implicit p: Predef[T]): Predef[Box[T]]`) and other modules' (E8's `NonEmptySeq` will get it for free); the core rule holds
(the element is resolved by the rule, the container is a Predef, so inlined). Cons: an extra implicit search for each applied type that reaches the
ADT families (a generic case class, `Either`, a tuple), plus one `Predef` search per type argument; four public-but-internal marker types in core;
tree surgery on Scala 3 (kept small, with the fallback).

### A'. Same retry, Scala 3 with a gated "hole" given instead of markers — LOST
`object Predef` gets `given derivationHole[T](using Gate[T]): Predef[T]`, where `Gate` is a transparent inline given that succeeds only for the argument
types the running retry registered (a ThreadLocal, like `AutoGuard`). The probe searches `Predef[NonEmptyList[P]]` itself, so it is exact (an `Order[P]`
is found too); the hole calls are replaced by the derived element. Works (spike), but: global mutable state with nesting rules (a derivation expanding
inside the search must not see the outer holes), a cast between `Quotes` instances to compare types, and it pollutes every user's failed `Predef`
search: `summon[Predef[X]]` now reports `I found: json.schema.Predef.derivationHole[X](???) But macro expansion was stopped.` (spike output).
Lost on that last point; it is the fallback if the marker approach meets a case it can't handle.

### B. A "container shape" type class in core — LOST
Core gets e.g. `trait ArrayLike[F[_]] { def unique: Boolean; def minItems: Option[Int] }` (and a map twin); cats declares `ArrayLike[NonEmptyList]` etc.;
both macros, on `F[T]` without an implicit, search `ArrayLike[F]` (no condition on `T`) and build `array(resolve(T))` with the validations.
Pros: simple searches, no probing, no tree surgery. Cons: new public API that users must learn; a second way to say what Predef constructors already say;
`NonEmptyMap[K, V]` (key pattern), `OneAnd[F, T]` (higher-kinded argument) need their own classes; doesn't help a user's own `Predef[F[T]]` constructors or
other modules. Lost: more API for less coverage.

### C. Teach both macros the cats shapes — LOST
Recognise `cats.data.NonEmptyList`, `NonEmptySetImpl.Type`, ... by name (no cats dependency) and build the arrays in the macros. Couples core/macros
to one library's internals (the newtype encodings changed in cats before), duplicates the cats module's validations in two macros, and does nothing
for other constructors.

### D. Derive in the cats module — LOST
Make cats' instances derive the element themselves (`implicit def nelDerived[T]: Predef[NonEmptyList[T]] = macro`/`inline`, calling the derivation).
Needs the cats module to depend on `api`/the macros, and runs a nested, separate derivation inside the implicit search: it loses the outer recursion
stack (`Tree(kids: NonEmptyList[Tree])` would loop), and on Scala 3 nested derivations inside a search are exactly what `AutoGuard` stops.

## Decision
Option A: when a type has no implicit and isn't a built-in collection, the macro probes for a Predef constructor with marker types standing in for the
arguments that have no `Predef`; if one exists, it applies it to the arguments resolved by the normal rule. Same semantics on Scala 2 and 3, no new user API.

## Implementation plan
0. **cats** (`CatsSupport.scala`, `ScalaVersionSpecificCatsSupport.scala`): the `Schema[T]` variants become non-implicit `@deprecated` defs (E1-R1;
   `nesSchema` also passes `unique = true`, so a direct call is right too); `chainSchemaFromPredef` (and the deprecated `chainSchema`) build a plain
   `array[T, Chain]` (no `minItems`). `CatsSupportSpec`: add `Chain[String]` (no `minItems`), and the existing `*EventSchema` cases with an element that
   has an implicit `Schema` (definition + `$ref`, `uniqueItems` for NES).
1. **core (shared)**: `com/github/andyglow/jsonschema/internal/Holes.scala`: `sealed trait Hole1..Hole4`, each with `implicit val predef: Predef[HoleK]`
   (a dummy schema, never rendered), INTERNAL API scaladoc. No change to `json.schema.Predef`.
2. **Scala 3** (`core/src/main/scala-3/.../derivation`):
   - new `PredefConstructors.scala` (trait mixed into `Derivation`): `derivePredefConstructor(tpe, ctx): Option[Node]`: applied type, not root when
     `skipRootSearch`; pick proper-type args (not higher-kinded) whose accepted `Predef` search fails (same acceptance as `lookupImplicit`, no failure recording);
     skip early when more than 4 args lack one (E1-R6); probe `Predef[F[..HoleK..]]` (`AutoGuard.searching`, no `recordFailure`); probe tree check;
     resolve each picked arg with `resolve(arg, ctx.descend(tpe))`.
   - new node `Constructed(tpe, probe: Term, holes: List[(Symbol, TypeRepr, Node)], extra)` in that file: `children` = the hole nodes (so recursion
     substitution and `as07`-style transforms see them); `emitRaw` substitutes types, replaces `HoleK.predef` refs with `Predef[a](emit(node))`, checks
     the result type, `.schema`. All checks that can fail run in `derivePredefConstructor`, before any node is built.
   - `Resolve.families`: insert `derivePredefConstructor` after `deriveArray`.
3. **Scala 2** (`macros/src/main/scala-2`): new `UPredefConstructors.scala` with an extractor `PredefConstructor(tpe)` used in `Macroses.resolve.genTree`
   after `Arr`; same arg picking (via `Implicit`'s Predef lookup, which handles `auto.derived`); silent marker probe (E1-R3); probe tree check;
   untypecheck + substitution; the `= null` block typecheck; new `SchemaType` `Constructed(tpe, body: Tree, holes: List[(TermName, Type, SchemaType)], extra)`
   emitting the local-val block; `transformSchema` descends into it.
4. **Tests**:
   - `modules/cats/.../CatsDerivedElementSpec.scala` (shared): NEL, NEV, NES, NEC, Chain, NEM[String, P], NEM[Int, P], OneAnd[List, P] of a derived `P`;
     as a field; nested `NonEmptyList[NonEmptyList[P]]`; recursive `Tree(kids: Chain[Tree])`; an element with an implicit `Schema` stays a `$ref`
     inside a constructor that only has a Predef variant (`OneAnd[List, P]`); NonEmptyLazyList/NonEmptyStream in the version-specific specs.
     (E1-R7, all in the shared cats spec unless noted) a sealed-family element `NonEmptyList[Animal]` rendered under the default profile and `Legacy`
     (the family node inside lowers like in `List[Animal]`: compare both renders with `List[Animal]`'s); `NonEmptyList[Option[P]]`; root recursion
     `Json.schema[NonEmptyList[Tree]]` with `Tree(kids: NonEmptyList[Tree])`; a field of a `derives json.Schema` case class (scala-3 test dir); a field
     of a `DerivedSchema` root (in module `derived`'s tests, which don't see cats: there with an api-side user constructor instead); an `import Json.auto._`
     block (NEL, NEV, Chain of `P`) (E1-R2); `NonEmptySet[X]`/`NonEmptyList[X]` with an implicit `Schema[X]` → `$ref` + definition (E1-R1); `Chain[String]`
     without `minItems`; a rendered draft-07 JSON of `NonEmptyList[P]` checked as text (`"type":"array"`, `"minItems":1`).
   - `api` (no cats): a user's own constructor `implicit def boxP[T](implicit p: Predef[T]): Predef[Box[T]]` with a derived element; one with a local
     val / lambda in its body (tree copying under `-Xcheck-macros`); constructors that also need `Ordering[T]` and `ClassTag[T]`, and a macro
     constructor: all fall back to structural (today's output) on both versions (E1-R4); a more specific constructor next to a generic one: both
     versions apply the one the probe found (E1-R4); a type with no constructor (`Box2[P]`) unchanged.
   - refined (E1-R3): its existing error specs on 2.12/2.13/3 pass unchanged; grep the test log for `Hole`.
   - Golden: no existing golden file uses cats or a Predef constructor of a derived element, so none should change; the full `api/test` run confirms.
   - Compile time (E1-R6): `api/Test/compile` after `clean`, 2.13 and 3, before and after, in the PR.
5. **Docs**: README "Cats" and paradox `modules/cats.md`: elements without an implicit are derived and inlined (example with a case class element);
   README/paradox section on custom Predefs: a Predef constructor (`implicit def f[T](implicit p: Predef[T]): Predef[F[T]]`) also applies to derived
   elements, as long as it needs nothing else about `T`.
6. Scala.js: nothing platform-specific; `rootJS/test` on all three versions (core/macros/api are touched).

## Compatibility
- Source: code that compiled keeps compiling; NEV/NES/NEC/NEM/NonEmptyLazyList of derived elements now compile.
- Output changes (all profiles, Legacy included): `NonEmptyList[P]`, `OneAnd[F, P]`, `NonEmptyStream[P]` (2.12) change from the `{head, tail}` object
  to an array with `minItems: 1`; `Chain[P]` from the oneOf of internals to an array; likewise any user type with a Predef constructor whose element was
  derived. Also an element with an implicit `Schema` (no `Predef`) in a constructor that only has a Predef variant (`OneAnd[F, T]`, 2.12's
  `NonEmptyStream`): today structural, then an array of `$ref`s. `Chain[T]` (any element) loses `minItems: 1`; `NonEmptySet[T]` with an implicit
  `Schema[T]` gains `uniqueItems: true`. These are the point of the fix. Legacy: the old output is a wrong schema (no codec writes `{head, tail}`), so by the lead's Legacy policy the
  fix applies there too; keeping the old output under Legacy isn't possible cheaply (the choice is made at compile time, profiles apply at render
  time), so the alternative is "no fix for Legacy users" only by an opt-out flag, which I don't propose.
- An element with an implicit `Schema` in any cats collection: inlined today, a definition + `$ref` after (E1-R1, the core rule; as `List[P]`).
- Source: `implicitly[Predef[NonEmptyList[P]]]` *outside* a derivation no longer compiles for a `P` that has only an implicit `Schema` (the
  `Schema[T]` variants aren't implicit any more); an explicit `CatsSupport.nelSchema[P]` call still compiles, with a deprecation warning.
- Binary: compatible (marker types added in core; the `implicit` flag dropped from the cats `Schema[T]` variants, which stay as methods;
  macro-internal classes). The `HoleK` types are public in bytecode and can show up in `-Xlog-implicits` / `-explain` output of a search the
  derivation runs (E1-R8). PVP: an output change → listed for the owner; the gap overview already files E1 under 0.10.0.
- Core rule: unchanged (implicit `Schema` → definition, `Predef` → inlined, else derive); this adds a step to "derive": a Predef constructor applied to
  the resolved arguments is preferred over structural derivation of the container.

## Review log
Round 1 (r-e1, on 7da5c36):
- E1-R1 (must) element with an implicit `Schema` inlined by cats' `Schema[T]` variants vs `$ref` elsewhere → accepted; escalated, lead: in scope,
  the rule wins, keep the variants if they can give `$ref`. They can't (no name at runtime), so they become non-implicit `@deprecated` defs; the hole
  path gives `$ref`. Problem text fixed; Compatibility lists the output and source change. Lead told; no objection so far.
- E1-R2 (must) `Json.auto` parity → accepted: section "`Json.auto`" (Scala 3 auto declines in the probe; Scala 2 unchanged, `inferPredef` reused), tests.
- E1-R3 (must) probe expanding refined's macro with markers → accepted: silent probe on Scala 2 (`c.typecheck(silent = true)` if `inferImplicitValue`
  reports aborts), success rejected by the probe tree check, refined error specs run unchanged + grep for `Hole`; result goes in the doc.
- E1-R4 (must) ClassTag and specificity parity → accepted, design changed: one probe tree check on both versions (markers only as type arguments and
  hole refs; no constants or macro expansions mentioning them), and Scala 2 now emits the probe's tree (untypecheck + substitution, local non-implicit
  vals) instead of `implicitly` under a local implicit, so both apply the same constructor. Tests for ClassTag, Ordering, a macro constructor, specificity.
- E1-R5 (nit) map keys without a Predef → accepted: "Known limitations".
- E1-R6 (nit) compile time → accepted: measured in the PR; early skip when more than 4 args lack a Predef.
- E1-R7 (must) tests → accepted, all added to step 4, except: the networknt check of a real `[{"a":1}]` → pushed back: the cats module has no
  validator dependency (it lives in api's test scope, with JS shims), and the claim is about the shape, which the rendered-JSON assertion pins
  (`"type":"array"`, `"minItems":1`). Adding a validator to cats for one assertion isn't worth the build change; happy to add it if you insist.
  The `DerivedSchema` field case goes in module `derived` with an api-side constructor (that module doesn't see cats).
- E1-R8 (nit) doc text → accepted: Problem wording, `HoleK` visibility in Compatibility, `derives`/`DerivedSchema` root in "Known limitations".
