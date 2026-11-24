# A generic implicit `Schema[T]` is used on Scala 3 but not on Scala 2  (issue #511, item B20)
Status: draft

## Problem
A generic implicit whose `Schema` type argument is fixed only by the expected type is used by the derivation on Scala 3
and ignored on Scala 2:

```scala
import json._

case class Q(b: Int)
case class P(a: Int, s: String, q: Q)

trait Low { implicit def anything[T]: Schema[T] = Schema.`string`.asInstanceOf[Schema[T]] }
object G extends Low {
  val p: Schema[P] = Json.schema[P]
}
```

`G.p` is `object(a: integer, s: string, q: object(b: integer))` on 2.12.21 and 2.13.18 and `def(P, string)` on 3.3.9
(the root `P` itself takes `anything[P]`). Found by r-b9 (`notes/reports/r-b9.md`, P4).

### Cause
`Schema` is covariant (`sealed trait Schema[+T]`). The macro searches `Schema[tpe]` and accepts the result only if it is a
`Schema` of exactly `tpe` (`UImplicits.isForExactly` / `Implicits.isExact`, owner decision 2026-11-20: an implicit
`Schema[Sub]` must not stand in for `Schema[Base]`). How the compilers instantiate a generic candidate:

- **scalac (2.12, 2.13)**: a type parameter that the expected type bounds only from above is instantiated as its lower
  bound, `Nothing`. `implicitly[Schema[Int]]` in plain user code compiles to `anything[Nothing]`
  (`-Xprint:typer`, `notes/scratch/c-b20/infer/`): a `Schema[Nothing]` conforms to `Schema[Int]`. The macro then sees
  `Schema[Nothing]`, rejects it as not exact, and goes on to `Predef`/derivation. If the candidate has an implicit
  parameter that mentions `T`, scalac leaves `T` undetermined for that search instead: with one instance (`Codec[Int]`)
  it works, with two (`Codec[Int]`, `Codec[String]`) `implicitly[Schema[Int]]` fails with "could not find implicit value",
  in plain user code too.
- **dotc (3.3)**: the macro's `Implicits.search` gives `anything[Int]` (exact, accepted), `vals[Int]` for
  `vals[T <: AnyVal]`, but `forQ[Nothing]` for a bounded `forQ[T <: Q]` in `Q`'s companion and `ord[Nothing]` for an
  F-bounded `ord[T <: Comparable[T]]`. A `summon[Schema[Q]]` in user code picks `forQ[Q]`, so Scala 3's macro search
  doesn't agree with Scala 3's own user-level resolution either (`notes/scratch/c-b20/s3search/`, `s3fbound/`).

`Predef` is invariant (`final case class Predef[T]`), so its generic implicits are instantiated exactly by both
compilers, and none of this applies to them.

### Actual output (master 4b2aff3, JVM; JS runs the same macros)
Probe: `notes/scratch/c-b20/B20ProbeSpec.scala` (put it in `api/src/test/scala/...` to run it). Each `GenN` holds the
implicit, imported (or inherited) where `Json.schema` is called. `P(a: Int, s: String, q: Q)`, `Q(b: Int)`.

| # | implicit in scope | 2.12.21 / 2.13.18 | 3.3.9 |
|---|---|---|---|
| G1 | `trait Low { implicit def anything[T]: Schema[T] }`, `object G1 extends Low` | `P`: `object(a: integer, s: string, q: object(b: integer))`; `Json.schema[Int]`: `integer` | `P`: `def(P, string)`; `Json.schema[Int]`: `def(scala.Int, string)` |
| G2 | the same, imported from an object | as G1 | as G1 |
| G3 | `implicit def anything[T]: Predef[T]` (invariant) | `string` | `string` |
| G4 | `implicit def tagged[T: ClassTag]: Schema[T]` | derived (as G1) | `def(P, string)` |
| G5 | `implicit def vals[T <: AnyVal]: Schema[T]` | derived (as G1) | `object(a: def(Int, string), s: string, q: object(b: def(Int, string)))` |
| G6 | `implicit def box[T]: Schema[Box[T]]` (`Box` invariant), field `Box[Int]` | `def(Box[Int], string)` | `def(Box[Int], string)` |
| G7 | `object Q7 { implicit def forQ7[T <: Q7]: Schema[T] }`, field `Q7` | `object(b: integer)` | `object(b: integer)` |
| G8 | `implicit def fromCodec[T](implicit c: Codec[T]): Schema[T]`, one `Codec[Int]` | `a: def(Int, string)`, rest derived | same |
| G9 | `implicit def listS[T]: Schema[List[T]]`, field `List[Int]` | `array(integer)` | `def(List[Int], string)` |
| G10 | `implicit def listP[T](implicit p: Predef[T]): Schema[List[T]]`, field `List[Int]` | `array(integer)` (and 2.13 warns "Unused import") | `def(List[Int], string)` |
| G11 | G8 with `Codec[Int]` and `Codec[String]` | derived (as G1) | `a: def(Int, string)`, `s: def(String, string)`, `q: object(b: def(Int, string))` |
| G12 | control: `implicit val subV: Schema[Sub12]`, field `Base12` (sealed, one member) | `family(Sub12: def(Sub12, string))` | same |

(`def(X, s)` is `def(signature = X, schema = s)`; field markers trimmed.)

## Why it is a problem
- The same source gives different schemas on Scala 2 and Scala 3 (G1, G2, G4, G5, G9, G10, G11), which the 0.8 port
  promised not to do ("Everything else is shared with Scala 2 and gives the same schemas", README "Scala 3 notes").
  Cross-built projects get different published schemas per Scala version.
- On Scala 2 the implicit *is* resolved by the compiler (`implicitly[Schema[Int]]` compiles, and at runtime the erased
  `anything[Nothing]` is the same value), so the user sees their implicit working everywhere except in the derivation.
  The exact-type rule was meant to stop a `Schema[Sub]` from standing for `Schema[Base]`, not to drop a generic
  implicit that the compiler instantiated at the bottom type.
- G7 is ignored on both versions although `summon`/`implicitly` of `Schema[Q7]` finds it: a "schema for every
  subtype of Q7" in Q7's companion is a natural thing to write.
- Realistic shapes: a catch-all fallback in a low-priority trait (G1), a schema for every member of a hierarchy in its
  companion (G7), a schema for a covariant container (`Schema[List[T]]`, `Schema[NonEmptyList[T]]`, G9/G10), a bridge
  from another type class (G8/G11: tapir, a codec, ...).

## Options

### A. Re-apply a generic implicit at exactly the target type, on both compilers  — WON
When the search returns a generic implicit (the found tree is `fun[targs](implicit args)`) that is **not** exact,
compute the type arguments that make its declared result exactly `Schema[tpe]` and re-apply it with them. Accept the
re-applied tree if it is valid (bounds hold, its implicit parameters resolve) and exact; otherwise decline as today.

- Solving: match the declared `Schema` argument of `fun`'s polymorphic type (`[T] => Schema[T]`,
  `[T] => Schema[List[T]]`) structurally against `tpe` (a type parameter binds to the type in the same place, type
  constructors must be the same symbol with the same arity; a conflicting binding fails). Type parameters that the
  match doesn't bind keep the compiler's arguments.
- Scala 2: `c.typecheck(q"${c.untypecheck(fun.duplicate)}[..$newTargs]", pt = Schema[tpe], silent = true)`. scalac
  checks bounds and resolves the implicit parameters (G4's `ClassTag[Int]`).
- Scala 3: `fun.appliedToTypes(newTargs)`, bounds checked against the `PolyType`'s `paramBounds`, then each implicit
  (`using`) clause applied with `Implicits.search` of its (instantiated) parameter types; any failure declines.
  `Symbol.info` is `@experimental` on 3.3, so bounds come from `fun.tpe.widen` (a `PolyType`), not from the symbols.
- **F-bounds** (a bound that mentions the method's own type parameters, `[T <: Comparable[T]]`): Scala 3 can't
  substitute `ParamRef`s in a bound with the public reflect API, so it declines them; Scala 2 declines them too (checked
  on the type parameters' bounds before the re-typecheck), so both versions agree (both decline today as well).
- The rule, as users will read it: **a generic implicit `Schema` counts for a type it can be instantiated at exactly.**
  The exact-type rule stays: a non-generic `Schema[Sub]` still doesn't stand for `Base` (G12), and a generic
  `sub[T <: Sub]` can't be instantiated at `Base` (bounds), so it doesn't either.

Result with the prototype (`notes/scratch/c-b20/prototype.patch`, both sides): every row of the table gives the
**3.3.9 column on all three versions**, except
- G7, which becomes `q: def(Q7, string)` (root: `def(Q7, string)`) on all three versions;
- G10 and G11, which stay as they are on Scala 2: scalac's search returns *nothing* for them, so there is no candidate to
  re-apply (see "What A can't fix" below).

(The prototype's Scala 2 side doesn't have the F-bound decline yet; the plan adds it.)
Full `clean test` with the prototype: 2.12.21, 2.13.18 and 3.3.9 JVM all green, no golden file changes (no test model
uses such an implicit; the library's own generic implicits are `Predef`s or fix `T` through an invariant type).

Pros: one rule on every version; the macro uses what the compiler resolves (Scala 2) or what `summon` resolves
(Scala 3, G7); small and local (one helper next to the exact check on each side, ~40 lines each); no API change.
Cons: an output change on Scala 2 for code that has such implicits (fields that were inlined become definitions with
the user's schema, as on Scala 3 already); G7 changes on Scala 3 too; G10/G11 remain a documented Scala 2 difference.

Variant **A-2 (Scala 2 only)**: re-apply on Scala 2, leave Scala 3 as it is. Simpler (no Scala 3 code), but G7 would
then be a definition on Scala 2 and derived on Scala 3: the version difference moves instead of going away. LOST.

#### What A can't fix (G10, G11)
For `listP[T](implicit p: Predef[T]): Schema[List[T]]` or `fromCodec[T](implicit c: Codec[T]): Schema[T]` with several
`Codec`s, scalac searches the implicit parameter with `T` undetermined, finds several candidates, and the whole
candidate fails: `c.inferImplicitValue` returns `EmptyTree`, exactly as `implicitly[Schema[List[Int]]]` fails in user
code. The macro has no candidate to re-apply. Making these work needs option D. A documents them as a Scala 2 limitation
with the workaround: give the type a non-generic implicit (`implicit val s: Schema[List[Int]] = ...`), or put `T` in an
invariant position (`Schema[Box[T]]` with `Box` invariant works, G6), or use a `Predef`.

### B. Scala 2's result as the reference: Scala 3 declines what Scala 2 can't resolve  — LOST
Make Scala 3 reject a generic implicit whose `Schema` argument depends on a type parameter that scalac would
instantiate as `Nothing` or leave undetermined. There's no exact way to compute "what scalac would do" on Scala 3; the
nearest rule ("a type parameter that occurs in the `Schema` argument and in no implicit parameter") gets G1/G2/G4/G5/G9
right but not G8 vs G11 (scalac's result depends on how many `Codec` instances exist) or G10.
Pros: Scala 2 output unchanged. Cons: removes behavior Scala 3 users have had since 0.8.0 (an output change on Scala 3
instead), approximate, and it keeps the macro disagreeing with the compiler on Scala 2 (the implicit resolves, the macro
ignores it). LOST.

### C. Keep the behavior, document the difference  — LOST
Add a "deliberate difference" to README "Scala 3 notes": a generic implicit `Schema[T]` whose `T` is fixed only by the
expected type is used on Scala 3 and ignored on Scala 2. No output change, no risk.
Cons: the difference is an accident of type inference, not a design decision, and G7 (ignored on both although
`summon` finds it) stays. The item exists to remove the difference. LOST, but it's the fallback if the owner doesn't want
any Scala 2 output change in a 0.9.x release: A's docs text minus the fix.

### D. Scala 2: enumerate the implicit candidates in the macro  — LOST
Collect the eligible implicits ourselves (lexical scope and the implicit scope of `Schema[tpe]`, through
`scala.tools.nsc` internals: `context.implicitss`, `implicitsOfExpectedType`), instantiate each at `tpe`, and replay
scalac's ranking. Would also fix G10/G11. Cons: compiler internals across 2.12.x/2.13.x, replaying the full ranking
(B9 replays one comparison and already needed care), and it would make the macro accept implicits that plain Scala 2
code can't resolve, which is a stronger claim than parity. LOST.

(Not an option: making `Schema` invariant. It would fix inference everywhere but breaks every `Schema[Sub]` used as
`Schema[Base]` in user code and the API; far outside this item.)

## Decision
A. When the found implicit is generic and not exact, the macro re-applies it at exactly the target type (bounds and
implicit parameters checked, F-bounds declined) on Scala 2 and Scala 3. The rule becomes "a generic implicit `Schema`
counts for a type it can be instantiated at exactly", the same on every version; what scalac can't resolve at all (G10,
G11) is documented as a Scala 2 limitation.

## Implementation plan
Prerequisites: implement after **B9 (#487)** and **B8 (#483)** are merged. Both rewrite `UImplicits.scala` (B9: the
self-reference handling, `infer`/`unlessSelfWins`; B8: the `isGenericImplicit` rename path in `get`). B20 adds a step
after the self-reference decline and before the `Predef` fall-back, so it composes with both. Start from
`notes/scratch/c-b20/prototype.patch` (written against master 4b2aff3) and port it onto the merged code.

1. **Scala 2, `macros/src/main/scala-2/com/github/andyglow/jsonschema/UImplicits.scala`**
   - Add `atExactly(x: Tree, tpe: Type, sType: Type): Option[Tree]`:
     peel `Typed` / `Block(Nil, _)` / `Apply` down to `TypeApply(fun, targs)`; require `fun.tpe` to be a
     `PolyType(tparams, res)` with `tparams.size == targs.size`; take `res.finalResultType.baseType(Schema).typeArgs.head`
     as the declared argument; solve it against `tpe` with a structural `unify` (type parameter → bind, same
     `typeSymbol` and arity → recurse into the arguments, a second, different binding → fail, anything else → no binding;
     both sides `dealias`ed); `newTargs` = the bindings, else the compiler's `targs`; if nothing changed → `None`;
     if a bound of a bound-to type parameter mentions any of `tparams` (F-bound) → `None`;
     otherwise `c.typecheck(q"${c.untypecheck(fun.duplicate)}[..$newTargs]", pt = sType, silent = true)`, and keep the
     result only if it is `isForExactly`.
   - In `lookupSchema`: the case `x if !isForExactly(x) => lookupPredef` becomes
     `atExactly(x, ...) map FromSchema getOrElse lookupPredef`. It stays *after* the self-reference cases (B9), so a
     declined self reference is never re-applied; the re-applied tree has the same symbol as the compiler's pick.
   - The re-applied tree is a `TypeApply`, so B8's `isGenericImplicit` path names the definition after `tpe` (that's the
     intent: `anything[Int]` → `def(scala.Int, ...)`).
   - The `Predef` search is untouched (invariant; already exact).
2. **Scala 3, `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/Implicits.scala`**
   - Add `atExactly(found: Term, tc: Symbol, tpe: TypeRepr): Option[Term]`: peel `Inlined(_, Nil, _)` / `Typed` /
     `Block(Nil, _)` / `Apply` to `TypeApply(fun, targs)`; `fun.tpe.widen` must be a `PolyType` with as many params;
     declared argument = `finalRes(pt.resType).baseType(tc)`'s argument (`finalRes` strips `MethodType`s); `unify`
     as on Scala 2 but binding `ParamRef`s of `pt` (`p.binder == pt`, by `paramNum`); bounds from `pt.paramBounds`:
     decline if a bound mentions a `ParamRef` of `pt` (F-bound), else `lo <:< t && t <:< hi`; then
     `applyImplicits(fun.appliedToTypes(newTargs))`: while the term's `tpe.widen` is a `MethodType` with
     `isImplicit`, search each `paramTypes` entry and `appliedToArgs` (a `match` on `ImplicitSearchSuccess`, not
     `isInstanceOf`: that warns "cannot be checked at runtime", as the prototype does); a non-implicit `MethodType` →
     `None`; finally `isExact`.
   - `searchAccepted`: a second success case, `s if !isSelfRef(s.tree) && !containsAuto(s.tree)` →
     `atExactly(s.tree, tc, tpe).map(Inlined(None, Nil, _))`. `lookupImplicit`'s `isGenericImplicit(tree)` is then true
     for it (the `FromImplicit.generic` rename, as for any generic implicit).
   - The derived tree passes `-Xcheck-macros` (the prototype ran the api tests with it).
   - Failures inside `atExactly` are not recorded by `recordFailure` (the first search succeeded; a decline here is a
     plain "doesn't apply").
3. **Tests**: a new shared spec `api/src/test/scala/com/github/andyglow/jsonschema/GenericImplicitParitySpec.scala`
   (runs on 2.12, 2.13, 3, JVM and JS), tree comparisons with **one expectation for every version** (Families.as07 not
   needed: no families except G12):
   - G1 (field and root `Json.schema[Int]`), G2, G4, G5 (`Int` field → definition; the case class `Q` → derived:
     `vals[Q]` violates `<: AnyVal`), G7 (field and root), G9: the 3.3.9 column / the new G7 result;
   - controls that must not change: G3, G6, G8, G12, and a generic `sub[T <: Sub]: Schema[T]` for a `Base` field
     (declined: bounds), and an F-bounded `ord[T <: Comparable[T]]: Schema[T]` for a `String` field (declined on both);
   - B9's P4 (`trait Low4 { implicit def anything[T]: Schema[T] }; object Mine4 extends Low4 { implicit val mine:
     Schema[X4] = Json.schema[X4] }`): `X4` derived (self reference declined), its `Int` field `def(scala.Int, string)`;
   - a sealed family member taken by a generic implicit (`USumTypes` / `Sums` use the same lookup);
   - G10 and G11 in a version-split spec (`scala-2/` and `scala-3/` test dirs, or one spec with `ScalaVersion` checks if the
     test harness has one) pinning the documented difference, so a future scalac change shows up.
   - One rendered-JSON check (Draft 07) for G1 so the definition name of `scala.Int` is covered end to end.
4. **Golden files**: none expected to change (prototype: full suites green). If one does, it's a side effect: stop and
   mail the lead.
5. **Docs** (README and paradox say the same):
   - README "Definitions and `$ref`s" and `docs/src/main/paradox/definitions.md`: after the core-rule list, a short
     paragraph: "A generic implicit counts for each type it can be instantiated at exactly: with
     `implicit def listSchema[T]: Schema[List[T]]` in scope, a `List[Int]` field is a definition named
     `scala.List[scala.Int]`. A `Schema[Sub]` still doesn't stand for `Schema[Base]`."
   - README "Scala 3 notes" and `docs/src/main/paradox/scala3.md`, deliberate differences: "**Generic implicits whose
     type parameter only an implicit parameter fixes** (`implicit def s[T](implicit c: Codec[T]): Schema[List[T]]`):
     Scala 2 can't resolve them when several `Codec`s exist (plain `implicitly` fails too), so the type is derived; Scala
     3 uses them. Workarounds: ...".
   - CHANGELOG: the lead's (output change list from the Compatibility section).
6. **JS**: nothing specific (pure macro code; the shared spec runs on `rootJS/test`).
7. **Gates**: as in the protocol (core and macros touched: JVM `clean test` on all three versions, `rootJS/test` on all
   three, `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`, `makeDocs`).

Risks:
- Scala 2 `c.typecheck(..., silent = true)` emits buffered warnings on success (noted in B9): a deprecated generic
  implicit warns once more at the call site, the same warning the original search gave. A candidate whose implicit
  parameters resolve to the enclosing val could trigger 2.13's `-Xlint:implicit-recursion` (B9's `-Wconf` makes it an
  error in our build); add a test with an implicit-parameter candidate inside `implicit val x: Schema[X] = ...` to see.
- `c.untypecheck` on `fun`: fine for `Select`/`Ident`/`This` prefixes (the prototype covers lexical, imported and
  companion candidates); a path-dependent prefix (`foo.bar.anything`) needs a test.
- Scala 3 `paramBounds` of a method in a class with its own type parameters are as seen from the prefix
  (`fun.tpe.widen`), so class type parameters are already substituted; an F-bound is the only declined shape.
- Compile time: `atExactly` runs only when a generic implicit was found and wasn't exact; no cost otherwise.

## Compatibility
- Source and binary: no public signature changes (`UImplicits` and `Implicits` are private to the macros).
- Output (all profiles, since it changes which implicit the derivation uses, not how the tree is rendered; Legacy's
  golden files don't change because no 0.7 model has such an implicit):
  - Scala 2.12/2.13: a type covered by a generic implicit that scalac instantiates at `Nothing` (G1, G2, G4, G5, G9) is a
    definition with that implicit's schema, as on Scala 3. Before: derived/inlined (or a `Predef`).
  - All versions: a bounded generic in a companion that both compilers instantiated at `Nothing` (G7) is used.
  - Unchanged: G10/G11 (documented as a Scala 2 limitation), every non-generic implicit, every `Predef`.
  - "Legacy keeps the old output" isn't available as a variant: profiles lower the tree at render time, and this is a
    derivation-time choice. Option C is the way to keep today's output.
- PVP: no API change; an output change for code with such implicits. Proposed as a patch-level fix (it removes a
  version-dependent result), but it changes Scala 2 schemas silently for those users: the owner may prefer to hold it for
  the next minor (0.10.0). Open question for the owner.

## Review log
