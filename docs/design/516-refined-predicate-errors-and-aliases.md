# refined: name the unsupported predicate on Scala 2, and accept aliased predicates  (issue #516, item B23)
Status: draft

## Problem

Two defects in the same piece of code, the refined module's predicate extractor (`Logic.gen` / `Extractors.P` on
Scala 2, `RefinedMacro.gen` / `ex` on Scala 3). Part 1 is the item as filed. Part 2 was found by r-b23 while
reviewing it, and the lead folded it in (keeping "own item" as the alternative, see Options).

### Part 1: Scala 2 reports a compiler-internal error for an unsupported predicate

```scala
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean._
import eu.timepit.refined.numeric._
import json.Json
import com.github.andyglow.jsonschema.RefinedSupport._

case class HasModulo(x: Int Refined Modulo[2, 3])          // 2.12: Modulo[W.`2`.T, W.`3`.T]
case class HasOptModulo(x: Option[Int Refined Modulo[2, 3]])

Json.schema[Int Refined Modulo[2, 3]]                       // a1
refined[Int Refined Modulo[2, 3]]                           // a2
implicitly[json.schema.Predef[Int Refined Modulo[2, 3]]]    // a3
Json.schema[HasModulo]                                      // a4
Json.schema[HasOptModulo]                                   // a5
Json.schema[Int Refined (Positive And Modulo[2, 3])]        // c1
```

Actual, 2.12.21 and 2.13.18, every line (JS too: it's the same macro, run by the compiler):

```
value asInstanceOf is not a member of <notype>
Note that <none> extends Any, not AnyRef.
Such types can participate in value classes, but instances
cannot appear in singleton types or in reference comparisons.
```

3.3.9: `refined[...]` and `implicitly[...]` report `Can't infer Predicate out of eu.timepit.refined.numeric.Modulo[2, 3]`.
Through the derivation (a1, a4, a5, c1): `schema for ...Refined[scala.Int, ...Modulo[2, 3]] is not supported, ⏎ an implicit for it failed: Can't infer Predicate out of ...`.

Cause: `modules/refined/src/main/scala-2/.../refined/Logic.scala:17,19` issues `warn("Can't infer Predicate out of " + showRaw(p))`
and returns `EmptyTree`. `RefinedMacro.forRefinedType` then emits `EmptyTree.asInstanceOf[json.Schema[T]]`, which is what fails to
typecheck. The warning itself never shows up in the compile output (checked: none of the lines above prints it), and if it did,
`showRaw` would print `TypeRef(ThisType(...), ..., List(ConstantType(Constant(2)), ...))`.

Control: an empty range (`Int Refined (Less[2] And Greater[7])`) already uses `err` (`c.abort`), and Scala 2 reports
`There are no values inside of specified range [2.0 .. 7.0]` on every entry point, including through implicit search and as a field.
So `c.abort` from this implicit macro does reach the user.

### Part 2: an aliased predicate is rejected (all three versions for some aliases)

```scala
object Preds {
  type Div2 = Divisible[2]
  type Pos  = Positive
  type Max  = MaxSize[2]
  type Two  = 2
}
Json.schema[Int Refined Preds.Div2]          // fails on 2.12, 2.13 and 3
Json.schema[List[Int] Refined Preds.Max]     // fails on 2.12, 2.13 and 3
Json.schema[String Refined MaxSize[Two]]     // fails on 2.12, 2.13 and 3 (the alias is the type argument)
Json.schema[Int Refined Preds.Pos]           // fails on 2.12 and 2.13, works on 3
```

r-b23's matrix (`notes/reports/r-b23.md`; FAIL = Part 1's message on Scala 2, `is not supported, ... Can't infer Predicate` on Scala 3):

| predicate as written | 2.12 | 2.13 | 3.3.9 |
|---|---|---|---|
| alias of `Positive`, `Negative`; an alias of that alias; `Pos And Positive` | FAIL | FAIL | ok |
| alias of `NonPositive`, `NonNegative`, `NonEmpty`, `GreaterEqual[2]`, `LessEqual[2]`, `NonDivisible[2]`, `MinSize[2]`, `Interval.Closed[2, 5]` | ok | ok | ok |
| alias of `Divisible[2]` | FAIL | FAIL | FAIL |
| alias of `MaxSize[2]` | FAIL | FAIL | FAIL |
| `MaxSize[Two]`, `Greater[Two]` (`type Two = 2`) | FAIL | FAIL | FAIL |
| `Positive`, `Divisible[2]`, `MaxSize[2]` written directly | ok | ok | ok |

Cause (both versions): the extractor tries the predicate as written, then fully dealiased
(`unmatch(p) orElse unmatch(p.dealias)`: `Extractors.scala:129`, `RefinedMacro.scala` `ex`), never the levels in between.
The matcher knows refined's own names (`Positive`, `Divisible`, `MaxSize` ...), which are themselves aliases, so a user alias
of one of them is matched only if refined's *expansion* is something the matcher also knows. Refined 0.11.4's expansions
(printed with Scala 2 reflection):

| refined alias | expands to (Scala 2) | matched after full dealias? |
|---|---|---|
| `Positive`, `Negative` | `Greater[shapeless.nat._0]`, `Less[shapeless.nat._0]` | Scala 2: no (`C` accepts only a `ConstantType`, `_0` is a `Nat`). Scala 3 (`Greater[0]`): yes |
| `Divisible[N]` | `Modulo[N, _0]` (Scala 3 `Modulo[N, 0]`) | no: `Modulo` has no case |
| `MaxSize[N]` | `Size[Interval.Closed[_0, N]]` | no: a two-sided bound inside `Size` isn't a `NumericPred` |
| `NonPositive`, `NonNegative`, `NonEmpty`, `NonDivisible[N]` | `Not[Positive]`, `Not[Negative]`, `Not[Empty]`, `Not[Divisible[N]]` | yes (`Not` matches, its argument is still written with refined's name) |
| `GreaterEqual[N]`, `LessEqual[N]` | `Not[Less[N]]`, `Not[Greater[N]]` | yes |
| `MinSize[N]`, `Interval.Closed[L, H]` | `Size[GreaterEqual[N]]`, `And[GreaterEqual[L], LessEqual[H]]` | yes |

A type-argument alias (`MaxSize[Two]`) fails for a different reason: `C` (the literal extractor) doesn't dealias its argument.

## Why it is a problem

- Part 1: the user can't tell which predicate is the problem, or that it is a predicate at all. Issue #58 (2020) is a user hitting
  the same text for `NonEmptyString`; it was closed by adding that predicate, so the message stayed. Every predicate outside the
  documented list hits it on Scala 2 (`Modulo`, `string.ValidInt`, ...), and so does any refined type
  reached through a field, an `Option`, a collection.
- Part 2: the docs (README "Refined", paradox `modules/refined.md`) list `Divisible`, `MaxSize`, `Positive` as supported, and giving
  a predicate a name (`type Div2 = Divisible[2]`) is ordinary refined style. The schema can't be generated at all; on Scala 2 the error
  is Part 1's message, so it isn't even clear that the alias is the problem. Nothing compiles wrongly: it's a compile error, never a wrong schema.

## Options

### Part 1

#### A. `c.abort` with the predicate as written  — WON
`Logic.gen`: replace both `warn(...); EmptyTree` with `err(...)` (`HasLog.err` = `c.abort(c.enclosingPosition, _)`), printing the type
with `toString` instead of `showRaw`:
```scala
case _ => err(s"Can't infer Predicate out of $p")    // was warn(s"... ${showRaw(p)}"); EmptyTree
...
case _ => err(s"Can't infer Refined out of $t")
```
Prototyped (2.12, 2.13): every line of the repro now reports `Can't infer Predicate out of eu.timepit.refined.numeric.Modulo[2,3]`
(c1: `... out of eu.timepit.refined.boolean.And[eu.timepit.refined.numeric.Positive,eu.timepit.refined.numeric.Modulo[2,3]]`), at the
`Json.schema` / `refined` / `implicitly` call site, the same way the empty-range error is reported. A user's own implicit
`Predef`/`Schema` for the type in lexical scope still wins (compiles before and after, the user's schema is used).
The text is Scala 3's (`p.show`), except that Scala 2's `Type.toString` writes no space after a comma in type arguments (`Modulo[2,3]`).
Pros: two lines; same behaviour as Scala 3 and as the empty-range path; nothing that compiles changes. Cons: none found.

#### B. Scala 3's full wording on Scala 2 (`schema for X is not supported, an implicit for it failed: ...`)  — LOST
Make the Scala 2 derivation (`macros/.../UImplicits`) catch a failing implicit and wrap its message, as Scala 3's `Resolve` does.
Needs a non-silent `c.inferImplicitValue` (or a second typecheck of `implicitly[...]`) in the core derivation, for every type, to fix
the message of one module. Scala 2 already reports refined's own message alone for the empty range (pinned in
`Scala3RefinedErrorsSpec`'s doc comment), so A is consistent with existing Scala 2 behaviour; B is B11's territory (#471) and a much larger change.

#### C. Fall back to the base type's schema with a warning  — LOST
`Int Refined Modulo[2, 3]` would become `{"type": "integer"}` (refined's constraint dropped) plus a warning. Pros: compiles.
Cons: silently drops a constraint (the warning isn't visible today at all, see Problem), differs from Scala 3 (an error) and from the
docs ("An unsupported predicate ... is a compile error", `modules/refined.md`). A user who wants that can already write an implicit
`Predef` for the type.

### Part 2

#### D. Try every alias level, one step at a time; `C` dealiases its argument  — WON
The extractor tries the predicate as written, then each alias expansion one step at a time, then (as today) fully dealiased:
```
ex(p) = unmatch(p) orElse dealias1(p).flatMap(ex) orElse unmatch(p.dealias)
```
So `Preds.Div2` → `Divisible[2]` is matched by refined's name before it becomes `Modulo[2, _0]`. `C.unapply` matches
`t.dealias` instead of `t`, so `MaxSize[Two]` reads `2`. One-step dealias:
- Scala 2 (no public one-step API): for `TypeRef(pre, sym, args)` with `sym.asType.isAliasType`, the alias body
  `sym.info.asSeenFrom(pre, sym.owner)` (unwrapping a `PolyType`) with `sym.asType.typeParams` substituted by `args`.
- Scala 3: `TypeRef` with `typeSymbol.isAliasType` → `translucentSuperType`; `AppliedType(tycon: TypeRef, args)` with an alias
  `tycon` → `tycon.translucentSuperType.appliedTo(args)` (beta-reduces the alias's type lambda one level).

Prototyped on 2.12, 2.13 and 3.3.9 (diff kept in `notes/scratch/c-b23/prototype-fix.diff`): 19 aliased forms, each compared with
the same predicate written directly, all equal and identical across the three versions, e.g. `Preds.Div2` → `integer {multipleOf: 2}`,
`Preds.Max` on a `List[Int]` → `maxItems: 2`, `MaxSize[Two]` on a `String` → `maxLength: 2`, an alias of an alias of `Positive` →
`exclusiveMinimum: 0`, a parametric alias (`type Gen[N] = Divisible[N]`), a class-member alias (`h.HD` with `val h = new Holder`).
All existing refined tests pass unchanged (2.13: 84, 2.12: 69, 3.3.9: 90, incl. the probe), no warnings.
Pros: one mechanism for every alias depth and every predicate the matcher knows, on both versions; refined's own aliases keep
being matched by name, which is how the extractor was designed (Scala 3 `RefinedSupport` doc comment). Cons: Scala 2 needs a
hand-written one-step dealias (8 lines).

#### E. Teach the matcher refined's expansions  — LOST
Add cases for what the full dealias reaches: `Modulo[N, 0]` → `multipleOf N`, shapeless `Nat` (`_0`) as a literal in `C` (Scala 2),
`Size` of a two-sided bound (`Size[Interval.Closed[_0, N]]` from `MaxSize`, `Bounded` isn't a `NumericPred` today), and `C` dealiasing.
Pros: also makes `Modulo[N, 0]` and `Greater[_5]` written directly work. Cons: three new matcher features to fix aliases of names the
matcher already knows; each future refined alias whose expansion isn't covered breaks again; new support is outside an error-message item.
(Those features can still be added later on their own, see Open questions.)

#### F. Part 2 as its own item  — LOST (the lead's alternative; easy to pick)
B23 would be Part 1 only. It lost because Part 2 is the same function (`ex` / `Ex.unapply`) and the same test files, one reviewer
already reproduced both, and Part 1's new error is exactly what a Part-2 user sees on Scala 2 after Part 1 alone. To pick F: drop
steps 4, 5, 7 and the first sentence of step 9; steps 1–3, 6 and 8 don't depend on them.

## Decision

Part 1: A, `c.abort` with refined's message and the type as written, so Scala 2 says what Scala 3 says. Part 2: D, the extractor
also tries each alias level one step at a time and its literal extractor dealiases, on both versions. Both are bug fixes with no
change to any schema that compiles today.

## Implementation plan

Scala 2 main (`modules/refined/src/main/scala-2/com/github/andyglow/jsonschema/refined/`):
1. `Logic.scala`: `case _ => err(s"Can't infer Predicate out of $p")` and `case _ => err(s"Can't infer Refined out of $t")`
   (no `showRaw`, no `EmptyTree`). `gen` then never returns `EmptyTree`; `RefinedMacro.forRefinedType` stays as it is.
2. `HasLog.scala`: `warn` has no other user (`grep -rn "warn(" modules/refined/src/main/scala-2`): remove it.
3. `RefinedMacro.scala` (Scala 2): no change.
4. `Extractors.scala`:
   - `C.unapply`: match `t.dealias` (a literal written through an alias, `MaxSize[Two]`).
   - add, next to `C`:
     ```scala
     // one alias step: `Div2` -> `Divisible[2]`, `Divisible[2]` -> `Modulo[2, _0]`; None if `p` is not an alias
     def dealias1(p: Type): Option[Type] = p match {
       case TypeRef(pre, sym, args) if sym.isType && sym.asType.isAliasType =>
         val body = sym.info.asSeenFrom(pre, sym.owner) match {
           case PolyType(_, res) => res
           case res              => res
         }
         Some(body.substituteTypes(sym.asType.typeParams, args))
       case _ => None
     }
     ```
   - `Ex.unapply`: `val v = unmatch(p) orElse dealias1(p).flatMap(unapply) orElse unmatch(p.dealias)`.

Scala 3 main (`modules/refined/src/main/scala-3/com/github/andyglow/jsonschema/refined/RefinedMacro.scala`):
5. - `C.unapply`: match `t.dealias`.
   - `ex`: `unmatch(t, p) orElse dealias1(p).flatMap(ex(t, _)) orElse unmatch(t, p.dealias)`; comment: "as written, then one alias
     step at a time (refined's own names are aliases: `type Div2 = Divisible[2]` must reach `Divisible`, not `Modulo[2, 0]`), then fully dealiased".
   - add:
     ```scala
     private def dealias1(p: TypeRepr): Option[TypeRepr] = p match {
       case p: TypeRef if p.typeSymbol.isAliasType                            => Some(p.translucentSuperType)
       case AppliedType(tycon: TypeRef, args) if tycon.typeSymbol.isAliasType => Some(tycon.translucentSuperType.appliedTo(args))
       case _                                                                 => None
     }
     ```
   Scala 3's error path is already right: no change.

Tests (`modules/refined/src/test/`; all compile-time, so they run unchanged under `refinedJS`):
6. New `scala-2/com/github/andyglow/jsonschema/refined/Scala2RefinedErrorsSpec.scala` (2.12 + 2.13; the Scala 2 twin of
   `scala-3/.../Scala3RefinedErrorsSpec.scala`), with `assertCompiles` and a local `errorOf` (api's `harness.CompileErrors` isn't
   on refined's test classpath: refined depends on `api`, not `api % "test->test"`):
   - unsupported predicate via `refined[...]`, `implicitly[Predef[...]]`, `Json.schema[...]`, a field, an `Option` field: each error
     includes `Can't infer Predicate out of <the predicate>` and not `asInstanceOf`;
   - nested: `Positive And M` → includes `Can't infer Predicate out of eu.timepit.refined.boolean.And[`;
   - empty range: unchanged message;
   - a user's implicit `Predef` for the unsupported type in lexical scope is used (`Json.schema[...] shouldBe` the user's schema).
   **Gotcha (found while prototyping):** inside an `assertCompiles` string, shapeless' ``W.`2`.T`` typechecks without any error on
   2.12 and 2.13 (`assertCompiles("refined[Int Refined (Less[W.`2`.T] And Greater[W.`7`.T])]")` passes), so such a test passes
   vacuously. Define the predicates as aliases in the spec's companion object (``type Mod23 = Modulo[W.`2`.T, W.`3`.T]``, real
   source, works on 2.12 and 2.13) and refer to them from the strings. The message then names the alias as written:
   `Can't infer Predicate out of com.github.andyglow.jsonschema.refined.Scala2RefinedErrorsSpec.Mod23`, which is what the test asserts.
   The prototype (`notes/scratch/c-b23/Scala2RefinedErrorsSpec.scala`) fails today with the `asInstanceOf` text and passes with steps 1–2.
7. New shared `scala/com.github.andyglow.jsonschema.refined/RefinedPredicateAliasSpec.scala` plus the aliases in
   `scala-2.13+/.../RefinedPredicateAliases.scala` (literal types, 2.13 + 3) and `scala-2.12-/.../RefinedPredicateAliases.scala`
   (``W.`2`.T``). One test per row of Part 2's matrix: the aliased schema equals the directly written one, and for the four that fail
   today (`Div2`, `Max`, `MaxSize[Two]`, alias of `Positive`) also equals the literal expected tree (e.g.
   ``integer.withValidation(`multipleOf` := 2)``), so that two equally wrong trees can't pass. Include an alias of an alias, a
   parametric alias, a class-member alias and an alias inside `And`/`Not`. The prototype's 19 cases are in
   `notes/scratch/c-b23/alias/` (`B23AliasModels.scala`, `B23AliasSpec.scala`).
8. `Scala3RefinedErrorsSpec`: no change needed (its doc comment "Scala 2 reports the refined macro's message alone" becomes true for
   this message too). Optionally add one alias error case (`type M = Modulo[2, 3]` → `Can't infer Predicate out of ...M`).

Golden files: none. The refined module has no golden files, and nothing in `api` or the bridges uses refined (checked with grep).

Docs (README "Refined" and paradox `docs/src/main/paradox/modules/refined.md`, same words):
9. After the list of supported predicates: "A predicate can be an alias, at any depth and also as a type argument
   (`type Div2 = Divisible[2]`, `MaxSize[Max]` with `type Max = 5`)." README also gets paradox's existing sentence
   "An unsupported predicate, or a range with no values (`Less[2] And Greater[7]`), is a compile error." (README lacks it).
   The Scala 3 notes (README "Scala 3", `scala3.md`) stay as they are.

Gates (when implemented, per the protocol): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on 2.13.18 and 3.3.9 (refined
only, but `refinedJS` runs the new specs); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `makeDocs` (README/paradox).

Risks:
- Scala 2 `dealias1` on unusual aliases (bounded or higher-kinded type parameters, aliases inherited from a trait): the prototype
  covers object members, class members (`asSeenFrom` of a `val` prefix) and a parametric alias. If one misbehaves, the trailing
  `unmatch(p.dealias)` still runs, so the worst case is today's behaviour, never a crash (no `require`/exception on this path).
- Scala 3 `translucentSuperType` also sees through opaque types: a predicate declared as an opaque type would now be matched by its
  right-hand side. Refined has none; if that's unwanted, skip `typeSymbol.flags.is(Flags.Opaque)`.
- Termination: alias chains are finite (a cyclic alias is a compile error before the macro runs).

## Compatibility

- Output: none. Part 1 changes only a compile error's text (the `EmptyTree` path never compiled). Part 2's walk runs only after
  `unmatch(p)` fails; every alias that matches today is matched by the walk at a refined name whose case yields the same
  normalised predicate as today's full dealias (checked: all existing refined tests unchanged, and the matrix's "ok" rows give
  the same tree before and after). Code that newly compiles: aliased predicates (Part 2's FAIL rows).
- Implicit precedence: unchanged. A user's `Predef`/`Schema` in lexical scope wins before and after. A user `Predef` placed in
  implicit scope (in the object that defines the alias) with `RefinedSupport._` imported never compiled before (the failing lexical
  macro fails the whole search, checked on 2.13 and 3), so no existing workaround can change.
- Source/binary: no public signature changes (`HasLog`, `Extractors`, `RefinedMacro` are `private[jsonschema]` or macro internals).
- PVP: patch (0.9.x). Legacy profile: no impact (no output change; refined isn't part of the derivation's profiles).

## Open questions for the owner

1. Part 2 inside B23 (D) or as its own item (F)? The plan is split so F is "drop steps 4, 5, 7 and step 9's first sentence".
2. Out of scope, could be their own items: `Modulo[N, 0]` written directly (= `Divisible[N]`) and shapeless `Nat` arguments on
   Scala 2 (`Greater[_5]`), both new support (Option E's features).
3. Neighbour (not B23, reported to the lead): the docs list `Xml` as a supported string predicate, but neither macro has a case for
   it and refined's core artifact has no `eu.timepit.refined.string.Xml` (it is in a separate refined module).

## Review log
