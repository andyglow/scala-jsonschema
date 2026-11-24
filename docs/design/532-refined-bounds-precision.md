# refined: numeric predicate literals keep their exact value  (issue #532, item B31)
Status: draft

## Problem

The refined module turns a numeric predicate (`Greater[N]`, `GreaterEqual[N]`, `Less[N]`, `LessEqual[N]`,
`Divisible[N]`, `NonDivisible[N]`, `Positive`, ..., and the intervals, which dealias to `And`) into
`minimum`/`maximum`/`exclusiveMinimum`/`exclusiveMaximum`/`multipleOf`. Both macros read the literal `N` as a `Double`
and use that `Double` for two jobs:

- **emission**: Scala 2 `AST.scala` lifts `vv: Double = v.asInstanceOf[Number].doubleValue()` into the tree
  (`Divisable.tree`, `Bounded.tree`, `Ge.tree`, `Le.tree`); Scala 3 `RefinedMacro.scala` passes `asNumber(v): Double` to
  `num(v: Double)` (line 188), which emits `java.lang.Double.valueOf(..)`;
- **normalisation**: `Inside`/`Outside` (used by `And` and `Or`) compare `vv`s; `math.min`/`math.max` (Scala 2
  `Math.scala`, Scala 3 `RefinedMacro.math`) compare same-type literals exactly but mixed-type ones (`Int` vs `Long`)
  through `doubleValue`; the empty-range error prints `vv`s.

A `Double` holds every integer only up to 2^53, so a bigger `Long` literal becomes a different number, and two
different `Long` literals can compare equal. refined 0.11.4's `WitnessAs` (how a literal reaches the base type) widens
only `Int` literals (to `Byte`/`Short`/`Long`/`BigInt`/`Float`/`Double`/`BigDecimal`) and `Double` literals (to `Float`/
`BigDecimal`), besides a literal of the base type itself; so a big `Long` literal is meaningful only on a `Long` base.

Repro (specs and logs in `notes/scratch/c-b31/`, not committed; r-b31's independent repro in `notes/scratch/r-b31/`).
Draft 07; the same text on 2.12.21 (`W.` literals), 2.13.18 and 3.3.9 on the JVM, and on 3.3.9 Scala.js (Bun):

| # | type | today | refined's meaning |
|---|---|---|---|
| 1 | `Long Refined Less[9007199254740993L]` | `"exclusiveMaximum": 9007199254740992` (rejects 9007199254740992) | `< 9007199254740993` |
| 2 | `Long Refined LessEqual[9223372036854775807L]` | `"maximum": 9.223372036854776E+18` (2^63: accepts a non-`Long`) | `<= Long.MaxValue` |
| 2b | `Long Refined GreaterEqual[9223372036854775807L]` | `"minimum": 9.223372036854776E+18` (rejects `Long.MaxValue`, the only value refined accepts) | `== Long.MaxValue` |
| 3 | `Long Refined GreaterEqual[-9223372036854775808L]` | `"minimum": -9.223372036854776E+18` (the right value, -2^63 is a `Double`; text only) | `>= Long.MinValue` |
| 4 | `Long Refined Divisible[9007199254740993L]` | `"multipleOf": 9007199254740992` | multiples of 9007199254740993 |
| 5 | `Long Refined Interval.Closed[9007199254740993L, 9007199254740995L]` | `"minimum": 9007199254740992, "maximum": 9007199254740996` (accepts both neighbours) | `[..993, ..995]` |
| 6 | `Long Refined Interval.Closed[9007199254740992L, 9007199254740993L]` | `"allOf": [{"minimum": 9007199254740992}, {"maximum": 9007199254740992}]` (the bounds compare equal, `Inside` misses; rejects ..993) | `[..992, ..993]` |
| 7 | `Long Refined (GreaterEqual[9007199254740993L] And LessEqual[9007199254740992L])` | compiles (an `allOf` like #6) | an empty range: compile error |
| 8 | `Long Refined (GreaterEqual[5] And GreaterEqual[9007199254740993L])` | `"minimum": 9007199254740992` (the right operand is picked, then emitted as a `Double`) | `>= ..993` |
| 9 | `Float Refined Greater[0.1f]` | `"exclusiveMinimum": 0.10000000149011612` | `> 0.1f` |
| 10 | `Float Refined Less[16777217]` | `"exclusiveMaximum": 16777217` | refined converts `16777217` to `Float`: `< 1.6777216E7f`, so `16777216f` is rejected |
| 11 | `Int Refined Greater[17]` | `"exclusiveMinimum": 17.0` | `> 17` |
| 12 | `BigInt Refined Greater[5]` | `"exclusiveMinimum": 5.0` | `> 5` |
| 13 | `BigDecimal Refined Positive` | `"exclusiveMinimum": 0.0` | `> 0` |
| 14 | `Double Refined Positive`, `Double Refined Greater[0.1]`, `Double Refined Divisible[0.1]` | `0.0`, `0.1`, `0.1` | as written |

Empty-range errors print `Double`s (Scala 3, `typeCheckErrors`; Scala 2 has the same code):
`Int Refined (Less[2] And Greater[7])` → `There are no values inside of specified range [2.0 .. 7.0]`;
`Long Refined (Less[9007199254740993L] And Greater[9007199254740999L])` → `[9.007199254740992E15 .. 9.007199254741E15]`.

A `Char` literal (`Char Refined Greater['a']`) is a compile error on both versions today and stays one.

### Relation to B19 (#512) and B27 (#526), both approved, neither implemented

**B19** makes the core's `Value.num` paths exact and adds typed `:=` overloads to the numeric keywords (its P1-D:
`:=(Int)`, `(Long)`, `(Float)`, `(Double)`, `(BigInt)`, `(BigDecimal)`, next to the inherited `:=(Number)`, whose
`java.lang.Long`/`BigDecimal`/`BigInt` cases become exact, P1-A). Its **step 7** changes refined's emission: "emit every
bound at the refined base type: integral bases as a `Long` literal, `Float` as a `Float` literal, everything else as a
`Double` literal, taken from the predicate's original value". B31 can't fix anything without B19's core change: today
every bound goes through `:=(x: Number)` and `doubleValue`.

**B27** (option D) replaces `Ge/Le.min/max`, `Bounded`, `Inside`, `Outside`, `Naked` and `math.min/max` with an interval
set (`Bound(v: Any, inclusive)`, `Interval`, `Ranges`, `union`/`complement`/`intersect`) compared with a new
`math.compare(l: Any, r: Any)`: same-type literals exactly, mixed `Number`s via `doubleValue`. It keeps the
`[2.0 .. 7.0]` message word for word and adds `There are no values that satisfy <the predicate>`.

What each combination leaves:

| landed | rows still wrong |
|---|---|
| B19 (core + step 7) only | 6, 7 (normalisation compares `Double`s), 10, 12, 13; messages print `Double`s; `Int Refined Less[1.5]` truncated to `1` |
| B27 only | 1–5, 8 (emission), 9–13; rows 6/7 normalise right but emit `Double`s |
| B19 + B27 | 10, 12, 13; integral messages print `2.0`; `Int Refined Less[1.5]` → `1`; a mixed-type `doubleValue` fallback in `math.compare` (exact in every reachable case, see option B) |
| B19 + B27 + B31 | none |

So B31 is small on top of both: one exact literal model for compare, emit and report, with an emission rule that
reproduces B19 step 7 on every case B19 lists (`Int Refined Greater[17]` → `17`, `Double Refined Greater[1.5]` → `1.5`,
`Double Refined Positive` → `0.0`, `Long Refined Less[9007199254740993L]` exact, `Int Refined Divisible[20]` → `20`,
`fs-example` `16`/`512`) and fixes the rest.

## Why it is a problem

- **Wrong schemas.** Rows 1, 2b, 4, 5, 6, 10: the schema rejects values refined accepts, or accepts values refined
  rejects. Row 2b rejects the only valid value. `Long` bounds near or beyond 2^53 are common for ids, nanosecond
  timestamps and `Long.MaxValue` caps. Row 2: `9.223372036854776E+18` is 2^63, outside `Long`. Row 3 is the right value
  in E-notation (text only).
- **Missing compile error.** Row 7: an empty range compiles to an `allOf` nothing satisfies, while the same range with
  small numbers is a compile error (documented in `docs/src/main/paradox/modules/refined.md`).
- **Inconsistent text.** Rows 11–13: integer bounds on `integer`/`BigInt` schemas print `.0`; the README and the
  paradox refined page already promise `{"type": "integer", "exclusiveMinimum": 17}`. Error messages print `2.0` for
  `Int` literals.
- Who hits it: refined users with `Long` bounds (wrong values), everybody with integral refined bounds (the `.0`).

## Options

### A. An exact literal model (`Lit`) for emission, comparison and messages — WON
Both macros replace a predicate's `v: Any` (and the `vv: Double` derived from it; on B27's base, `Bound.v: Any`) with a
`Lit`, built once per literal from the refined base type `t` and the literal value:

```scala
// macro-time only (Scala 2: inside the cake, Scala 3: inside RefinedMacro)
final case class Lit(value: BigDecimal, kind: Lit.Kind) extends Ordered[Lit] {
  def compare(o: Lit): Int = value compare o.value
  def show: String = kind match {                  // how the base type prints it (error messages)
    case Lit.Integral => value.bigDecimal.toPlainString        // 2, 9007199254740993
    case Lit.Float32  => value.toFloat.toString                // 0.1, 2.0, 1.6777216E7
    case Lit.Float64  => value.toDouble.toString               // 0.1, 2.0, 9.007199254740992E15
    case Lit.Decimal  => value.toString                        // 0.1, 2
  }
}
object Lit {
  sealed trait Kind; case object Integral extends Kind; case object Float32 extends Kind
  case object Float64 extends Kind; case object Decimal extends Kind
  def apply(t: <Type>, v: Any): Lit   // the table below; a non-number literal: err("refined: $v is not a number")
}
```

**Value** = the literal as refined's `WitnessAs` converts it to the base type (refined 0.11.4
`internal/WitnessAs.scala`), held exactly:

| base type `t` | kind | value |
|---|---|---|
| `Byte`, `Short`, `Int`, `Long`, `BigInt`, `java.lang.{Byte,Short,Integer,Long}`, `java.math.BigInteger` | `Integral` if the value is whole, else `Decimal` | `Int`/`Long` literal: exact. A `Double`/`Float` literal (no `WitnessAs`; the type is legal): its exact decimal value, so `Less[1.5]` stays `1.5` |
| `Float`, `java.lang.Float` | `Float32` | the literal `.toFloat` (refined's `intWitnessAsFloat`/`doubleWitnessAsFloat`), as the `Float`'s shortest decimal (`BigDecimal(f.toString)`) |
| `Double`, `java.lang.Double` | `Float64` | the literal `.toDouble` (exact for `Int`; a `Long` beyond 2^53 is rounded, as refined would if it accepted it), as `BigDecimal(d)` (the shortest decimal) |
| `BigDecimal`, `java.math.BigDecimal` | `Decimal` | `Int`/`Long` exact; `Double` as `BigDecimal(d)` (refined's `doubleWitnessAsBigDecimal`); `Float` as its shortest decimal |
| anything else: a user type with a `Numeric`, **and the collection inside `Size[P]`** (`String`, `List[_]`: `Size`'s predicate is extracted with the refined type's base) | the literal's own: `Int`/`Long` → `Integral`, `Float` → `Float32`, `Double` → `Float64` | the literal, exact |

Shortest-decimal forms preserve order (distinct `Float`s/`Double`s get distinct, equally ordered decimals), so
comparing `value`s is exact for every kind. `compare` is by value and deliberately has no `require(kind == o.kind)`:
all `Lit`s of one predicate share `t`, so their kinds agree except on the integral row (`Integral` and `Decimal` for
`Int Refined (Greater[1] And Less[1.5])`) and inside `Size[P]` (`Size[Greater[2] And Less[5.5]]`), where comparing the
exact values is still right.

**Emission** picks B19's typed `:=` overload from `kind`: `Integral` → a `Long` literal if the value fits a `Long`,
else `BigDecimal("<digits>")` (no literal can exceed a `Long` today; the branch is for completeness); `Float32` → a
`Float` literal; `Float64` → a `Double` literal; `Decimal` → `scala.math.BigDecimal("<digits>")`. B19 renders each
exactly (`17`, `9007199254740993`, `0.1`, `16777216`, `0.0`, `0`).

**Normalisation**: every comparison uses `Lit`'s ordering; `math` (`min`/`max` today, B27's `compare`) goes. The
inclusiveness logic and the shapes are not touched: today's or B27's, whichever is in place.

**Messages**: every value in a message is `lit.show`.

Output (Draft 07; JVM and JS; all profiles):

| # | type | today | A |
|---|---|---|---|
| 1 | `Long Refined Less[9007199254740993L]` | `9007199254740992` | `9007199254740993` |
| 2/2b/3 | `Long Refined LessEqual[Long.MaxValue]` / `GreaterEqual[Long.MaxValue]` / `GreaterEqual[Long.MinValue]` | `9.223372036854776E+18` / the same / `-9.223372036854776E+18` | `9223372036854775807` / the same / `-9223372036854775808` |
| 4 | `Long Refined Divisible[9007199254740993L]` | `9007199254740992` | `9007199254740993` |
| 5 | `Interval.Closed[..993, ..995]` | `..992`, `..996` | `..993`, `..995` |
| 6 | `Interval.Closed[..992, ..993]` | `allOf[{minimum ..992},{maximum ..992}]` | `{"type": "integer", "minimum": 9007199254740992, "maximum": 9007199254740993}` |
| 7 | `GreaterEqual[..993] And LessEqual[..992]` | compiles | `There are no values inside of specified range [9007199254740993 .. 9007199254740992]` |
| 8 | `GreaterEqual[5] And GreaterEqual[..993]` | `..992` | `..993` |
| 9 | `Float Refined Greater[0.1f]` | `0.10000000149011612` | `0.1` |
| 10 | `Float Refined Less[16777217]` | `16777217` | `16777216` (the `Float` refined compares against; B19's `Decimals.fromFloat(16777216f)` = `BigDecimal("1.6777216E7")`, scale 0) |
| 11 | `Int Refined Greater[17]` | `17.0` | `17` |
| 12 | `BigInt Refined Greater[5]` | `5.0` | `5` |
| 13 | `BigDecimal Refined Positive` | `0.0` | `0` |
| 14 | `Double Refined Positive` / `Greater[0.1]` / `Divisible[0.1]` | `0.0` / `0.1` / `0.1` | unchanged |
| — | `Int Refined Less[1.5]` (not validatable by refined) | `1.5` | `1.5` (B19 step 7 as written: `1`) |
| — | message for `Int Refined (Less[2] And Greater[7])` | `[2.0 .. 7.0]` | `[2 .. 7]` (a `Double` base keeps `[2.0 .. 7.0]`) |

Pros: every number is the one refined compares against, for every job, from one constructor per macro; both macros keep
the same structure (they are a port of each other); comparison can't drift from emission. Cons: every literal-holding
field changes type (`Ge`/`Le`/`Divisable` today, `Bound` on B27's base), in code B27 also rewrites (plan step 0).

### B. B19 step 7 + B27's `math.compare`, no `Lit` — LOST
Keep `v: Any` for comparisons and messages (B27's `math.compare`: same type exact, mixed `Number`s via `doubleValue`) and
emit at the base type from the original literal (B19 step 7 as written). With both landed, rows 1–9 and 11 are right:
same-type `Long` literals compare exactly, and the mixed `doubleValue` fallback is exact in every reachable case (an
`Int` literal is exactly a `Double`; refined widens no `Long` literal, so a `Long` is only compared with `Int`s and
`Long`s). What B leaves, and why A wins:
- row 10: an `Int` literal on a `Float` base keeps `16777217`, which accepts `16777216f` (refined rejects it);
- rows 12/13: `BigInt`/`BigDecimal` bases keep `5.0`/`0.0` ("everything else as a `Double` literal");
- `Int Refined Less[1.5]` (a legal type, not validatable by refined) is truncated to `1`, which changes its meaning;
- messages print `2.0` for `Int` literals;
- two notions of a literal's value (one to compare, one to emit) and a `doubleValue` fallback that is exact only
  because of what refined happens to widen.
Each of these is small; together they are most of what A changes, and A costs one small type per macro. Lost on those
points (not on rows 6/7, which B27 alone fixes).

### C. Exact comparisons on the raw literal, no conversion to the base type — LOST
Keep `v: Any`, but compare through `BigDecimal` (`toBig(v: Any)` in the comparisons) and emit with B19 step 7. Fixes
everything A fixes except row 10 and, unless step 7's rule is extended anyway, rows 12/13. It is A without the `Lit`
type: the conversion and kind logic would be spread over the comparisons and the emitters instead of one constructor.
No real saving over A; lost.

### D. Reject literals a `Double` can't hold — LOST
A compile error for a `Long` literal beyond 2^53 ("refined bound 9007199254740993 can't be represented"). Honest, tiny,
but refined accepts these types and users have them (ids, `Long.MaxValue` caps): a regression from "slightly wrong"
to "doesn't compile". Lost.

### E. Legacy keeps the old output — LOST (not possible)
refined builds its schema tree at compile time and the tree has no profile: `JsonSchemaProfile` lowers only `option` and
`family` nodes at render time (B27 reached the same conclusion). Keeping `17.0` or 2^63 under Legacy would need refined
to emit a profile-dependent number node for what are value fixes (rows 1–8, 10) and B19's number layout (rows 11–13).
The lead's Legacy policy (fixes that make a schema correct apply under Legacy too) covers it. No golden-legacy file holds
refined output.

## Decision

A: both refined macros build one exact `Lit` per numeric literal (the value refined's `WitnessAs` gives on the base
type, plus how the base type prints it) and use it to compare, normalise, report and emit; emission goes through B19's
typed `:=` overloads. It reproduces B19 step 7 on every case B19 lists and replaces step 7's rule where step 7 is lossy
or changes the meaning.

**B31 is implemented as B19's step 7**: the B19 implementation PR does refined's part as this doc's plan instead of
#518's step 7 text (#518's step 7 gets a "superseded by #532" line; asked of the lead). The B19 PR has to touch refined's
macros anyway (otherwise `fs-example` renders `16.0` on Scala 2 and `16` on Scala 3), so one change instead of two.

## Implementation plan

0. **Order and bases.**
   - **With B19.** This plan is B19's step 7. It needs B19's core (typed `:=` overloads, exact `num(x: Number)`,
     `Decimals.fromFloat`) in the same PR or before it. If B19's implementer can't take it, a follow-up PR right after
     B19 does it, and B19's PR does step 7 as #518 wrote it (then this plan rewrites those lines); the lead decides.
   - **With B27.** B27-D deletes the functions today's code compares in. Steps 1–2 below give the change on **both
     bases**: (a) today's code, (b) on top of B27-D. Whichever of B19-with-B31 and B27 lands second applies the other
     base's version (a local rebase: the field types and the comparison helper).
   - **Message conflict.** B27 keeps `There are no values inside of specified range [2.0 .. 7.0]` word for word and
     pins it in `Scala3RefinedErrorsSpec`; B31 makes it `[2 .. 7]` (`show`). The PR that lands second updates the pins
     (B27's new `There are no values that satisfy <pred>` shows the predicate type, so it has no number to format).
     c-b27 and the lead are told (mail, 2026-11-23).
   - **B32** (`Size` exclusive bounds ±1) reads the `Lit` (`lo.v.value + 1`, exact). **D2** (`Short`/`Byte`/`BigInt` →
     `integer`) changes `isInt`/`prefix`; independent of `Lit.kind`.
1. **Scala 2 `modules/refined/src/main/scala-2/com/github/andyglow/jsonschema/refined/`**, both bases:
   - new `Literals.scala`: `private[jsonschema] trait Literals { this: HasContext with HasLog => ... }` with `Lit`,
     `Lit.Kind`, `Lit.apply(t: Type, v: Any)` (the table in A; base types matched with `=:=` against `typeOf[...]`), `Lit.show`,
     and `def litTree(l: Lit): Tree` (`Integral` → `Literal(Constant(l.value.toLong))` when `l.value.isValidLong`, else
     `q"_root_.scala.math.BigDecimal(${l.value.bigDecimal.toPlainString})"`; `Float32` → `Literal(Constant(l.value.toFloat))`;
     `Float64` → `Literal(Constant(l.value.toDouble))`; `Decimal` → `q"_root_.scala.math.BigDecimal(${l.value.bigDecimal.toPlainString})"`).
     A non-`Number` literal: `err(s"refined: $v is not a number")` (Scala 3's `asNumber` message; Scala 2 today casts
     with `asInstanceOf[Number]` inside the macro; both already fail to compile `Char Refined Greater['a']`).
   - `Logic.scala`: the cake `extends Extractors with HasLog with AST with Literals with HasContext` (drop `Math`);
     `AST`'s self-type gains `Literals`, loses `Math`. Delete `Math.scala`.
   - `Extractors.scala`: `Ge(t, Lit(t, v))` etc. for `sGreater`, `sGreaterEqual`, `sLess`, `sLessEqual`, `sDivisable`,
     `sNonDivisable`; `Pos/NonPos/Neg/NonNeg` use `Lit(t, 0)`.
   - **(a) today's `AST.scala`**: `NumericPred.v: Lit` (drop `vv`); `Divisable(t, v: Lit)`, `Ge(t, v: Lit, ...)`,
     `Le(t, v: Lit, ...)`; every `:= $vv` / `:= ${min.vv}` becomes `:= ${litTree(..)}`; `asSize` =
     `Size.Def.Min(v.value.toInt)` / `Max(...)`; `Ge/Le.min/max` take `if (v <= o.v) v else o.v` (resp. `>=`) instead of
     `math.min/max`, inclusiveness merge untouched; `Inside`/`Outside` compare `l.v < r.v` etc.; the `And` error uses
     `min.v.show`/`max.v.show`.
   - **(b) on B27-D**: `Bound(v: Lit, inclusive)`; every `math.compare(a, b)` → `a compare b` (B27 step 1's helper
     goes, with `Math.scala`); `Ranges.tree` emits each bound with `litTree`; `Divisable(t, v: Lit)` as in (a); B27 step 6's
     `Size` reads `lo.v.value.toInt` / `hi.v.value.toInt`; both of B27's messages format values with `show`.
2. **Scala 3 `modules/refined/src/main/scala-3/com/github/andyglow/jsonschema/refined/RefinedMacro.scala`**, both bases:
   - `Lit` as a private case class of `RefinedMacro` (base-type tests `t =:= TypeRepr.of[Long]` etc.);
     `Lit(t: TypeRepr, v: Any)` replaces `asNumber`.
   - `num(v: Double): Expr[Number]` goes; `minimum(l: Lit, inclusive)`, `maximum(l: Lit, inclusive)`, `multipleOf(l: Lit)`
     match on `l.kind` and splice a typed literal so the call resolves to B19's overload:
     `'{ Instance.`minimum` := ${Expr(l.value.toLong)} }` (`Long`), `${Expr(l.value.toFloat)}`, `${Expr(l.value.toDouble)}`,
     `'{ Instance.`minimum` := scala.math.BigDecimal(${Expr(s)}) }`. Their result type stays `Expr[Def[Number, ?]]`.
   - (a) / (b): as in step 1 (`Ge`/`Le`/`Divisable` fields, `Pos`/`Neg`, `asSize` or B27's `Size`, comparisons, messages);
     delete `private object math`.
   - the class scaladoc: replace "Bounds and `multipleOf` are emitted as Doubles" with "a numeric literal is read as
     refined converts it to the base type and emitted exactly at that type".
3. **Tests**, refined module (shared dirs run on JVM and JS):
   - new `modules/refined/src/test/scala-2.13+/com.github.andyglow.jsonschema.refined/RefinedPrecisionSpec.scala`
     (literal types: 2.13 and 3): every row of A's output table as rendered Draft 07 text (`stringify` + a
     whitespace-normalised `include`, as in the repro), including row 6's full object and row 14 unchanged, plus
     `Int Refined Less[1.5]` → `1.5` and `Short Refined Greater[5]` → `5`. On B27's base, row 6 is B27's one-interval
     `Ranges`: same text.
   - new `modules/refined/src/test/scala-2.12-/com.github.andyglow.jsonschema.refined/RefinedPrecisionSpec.scala`: the
     same rows with ``W.`...`.T`` literals (rows 1, 2, 2b, 5, 6, 9, 10, 11, 12 at least).
   - empty range by precision (row 7) and the message format: Scala 3 `Scala3RefinedErrorsSpec`: update the three
     `[2.0 .. 7.0]` expectations to `[2 .. 7]`, add row 7's message and a `Double` base (`Double Refined (Less[2] And
     Greater[7])` → `[2.0 .. 7.0]`). Scala 2: probe `shapeless.test.illTyped` first (shapeless is on refined's Scala 2
     classpath; it typechecks with `c.typecheck` and should see the macro's `c.abort`, but `RefinedBooleanSpec` notes
     scalatest didn't); if it works, a new
     `modules/refined/src/test/scala-2/com.github.andyglow.jsonschema.refined/Scala2RefinedErrorsSpec.scala` with
     ``W.`...`.T`` literals (one file for 2.12 and 2.13) for the same three messages; if not, a comment, as B27 step 8 does.
   - if B27's (or B14's) `RefinedRangesValiditySpec` with networknt is in place, add `Long` validity rows there:
     x = 2^53, 2^53+1, `Long.MaxValue` against rows 1, 2b, 5, 6 (JVM; canceled on JS).
   - existing tree specs (`RefinedNumbersSpec`, `RefinedBooleanSpec`, `RefinedQuirksSpec`, `RefinedAliasedSpec`,
     `RefinedFieldsSpec`): no change expected (`Def` equality compares the `Value`, and `num` equality is numeric). If
     one fails, the failure is a real output change: report it.
4. **Golden files**: `modules/refined/src/test/resources/refined/fs-example.draft07.json`: `16.0`/`512.0` → `16`/`512`
   (`Int` base; B19's expected change too). No `api` golden or `golden-legacy` file uses refined.
5. **Docs** (README and paradox say the same): README "Refined" section (after the supported-predicates list) and
   `docs/src/main/paradox/modules/refined.md` (before "An unsupported predicate..."): add
   "Numeric bounds keep the literal's exact value, as refined reads it for the base type: `Long Refined
   Less[9007199254740993L]` gives `"exclusiveMaximum": 9007199254740993`, `Int` bounds print as integers (`17`), `Double`
   bounds as `Double`s (`Double Refined Positive` gives `0.0`)." No CHANGELOG edit (the lead's).
6. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9 (`clean`: the macros change); `rootJS/test`
   on 2.13.18 and 3.3.9 (`refinedJS` at least; all three with B19's core); `scalafmtCheckAll` (+ `rootJS/scalafmtCheckAll`);
   `makeDocs` (README and paradox change).

Scala 2 vs 3: the same `Lit` table in both, tested by the shared `scala-2.13+` spec on 2.13 and 3 and by the shared
golden. Scala 2 emits `Literal(Constant(...))` trees, Scala 3 `Expr(...)`; both make the static type of the right-hand
side `Long`/`Float`/`Double`/`BigDecimal`, which is what selects B19's overload. Scala.js: the macros run on the JVM at
compile time (`BigDecimal` there is fine); the emitted `Long` literal is a `RuntimeLong` and B19's `:=(Long)` keeps it
exact; `Float` literals render through B19's JS `Decimals.fromFloat`.

Risks: the `Float`-base conversion (row 10) changes a bound's value (16777217 → 16777216), deliberately, to match
refined; `-Xcheck-macros` on Scala 3 (spliced literals are plain constants, low risk); a user alias of a base type
(`type Id = Long`): `=:=` sees through it, as `isInt` already relies on; the rebase between the (a) and (b) bases.

## Compatibility

- Source/binary: no public API change (the macros and their helpers are `private[jsonschema]`). The code the macros
  emit at user call sites changes and needs B19's typed `:=` overloads (same release).
- Output (all profiles, Legacy included, see option E; JVM and Scala.js): as A's table. `Long` bounds exact; integral
  bases print integers (`17`, also B19's); `BigInt`/`BigDecimal` bases print whole bounds as integers (`5`, `0`); `Float`
  bases print the `Float` refined compares against (`0.1`, `16777216`); `Double` bases unchanged; row 6 a proper
  interval instead of an `allOf`. Golden: `refined/fs-example.draft07.json` only (shared with B19). No `golden-legacy`
  change.
- Compile errors: row 7 newly fails to compile (it accepted no value anyway; B27 widens the same rule); the empty-range
  message prints `[2 .. 7]` instead of `[2.0 .. 7.0]` for integral bases.
- PVP: no API change; a patch by the API rule, riding B19's release (the lead decides whether the text changes count as
  breaking, as for B19).

## Review log
Round 1 (r-b31, on edc3342):
- B31-R1 (must) B27-D deletes the code the plan edits; plan both bases; flag the message conflict → accepted: Problem
  gets a B27 paragraph; plan steps 1–2 give (a) today's code and (b) B27-D (`Bound.v: Lit`, `math.compare` → `Lit`
  ordering, `Ranges.tree` via `litTree`, B27's `Size` reads `lo.v.value`, both messages via `show`); step 0 states the
  `[2.0 .. 7.0]` → `[2 .. 7]` conflict (second to land updates the pins); mailed c-b27 and the lead.
- B31-R2 (must) re-judge B against B27 → accepted: B is now "B19 step 7 + B27's `math.compare`"; it fixes rows 6–8 too;
  the verdict names what's left (rows 10, 12, 13, `Less[1.5]`, messages, two value notions). The Problem has a
  "what each combination leaves" table, including "B27 only".
- B31-R3 (must) one B19 path → accepted: Decision says B31 is implemented as B19's step 7 in the B19 PR (#518's step 7
  gets "superseded by #532", asked of the lead); fallback (a follow-up PR) only if the lead says so.
- B31-R4 (must) row 10 renders `16777216` → accepted: A's table, Compatibility; the `show` (`1.6777216E7`) stays.
- B31-N5 (nit) → accepted: row 2b (`GreaterEqual[Long.MaxValue]` rejects its only value; r-b31's logs); row 3 text-only.
- B31-N6 (nit) → accepted the `Size` note (the "anything else" row names the collection base); pushed back on
  `require(kind == o.kind)`: kinds legitimately differ on an integral base with a non-whole literal and inside `Size[P]`
  with mixed literals, and comparing exact values is right there; A says so.
- B31-N7 (nit) → accepted: option E "Legacy keeps the old output — LOST (not possible)".
- B31-N8 (nit) → accepted: step 3 probes `illTyped` first, fallback a comment.
- B31-N9 (nit) → accepted, conditional: `Long` validity rows if B27/B14's validity spec is in place.
