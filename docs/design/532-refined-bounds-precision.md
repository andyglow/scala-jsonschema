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
different `Long` literals can compare equal.

Repro (specs and logs in `notes/scratch/c-b31/`, not committed). Draft 07; the same text on 2.12.21 (`W.` literals),
2.13.18 and 3.3.9 on the JVM, and on 3.3.9 Scala.js (Bun):

| # | type | today | refined's meaning |
|---|---|---|---|
| 1 | `Long Refined Less[9007199254740993L]` | `"exclusiveMaximum": 9007199254740992` (rejects 9007199254740992) | `< 9007199254740993` |
| 2 | `Long Refined LessEqual[9223372036854775807L]` | `"maximum": 9.223372036854776E+18` (2^63) | `<= Long.MaxValue` |
| 3 | `Long Refined GreaterEqual[-9223372036854775808L]` | `"minimum": -9.223372036854776E+18` | `>= Long.MinValue` |
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

### Relation to B19 (#512, design approved, not implemented)

B19 makes the core's `Value.num` paths exact and adds typed `:=` overloads to the numeric keywords (its P1-D:
`:=(Int)`, `(Long)`, `(Float)`, `(Double)`, `(BigInt)`, `(BigDecimal)`, next to the inherited `:=(Number)`, whose
`java.lang.Long`/`BigDecimal`/`BigInt` cases become exact, P1-A). Its **step 7** already changes refined's emission:
"emit every bound at the refined base type: integral bases as a `Long` literal, `Float` as a `Float` literal,
everything else as a `Double` literal, taken from the predicate's original value". That fixes rows 1–4, 9 and 11 of the
table. It leaves:

- **normalisation** (rows 5's comparison side, 6, 7, 8's comparison side): `Inside`/`Outside` and mixed-type
  `min`/`max` still compare `Double`s;
- **the error message** (prints `Double`s);
- **base-type conversions** refined itself applies (row 10: an `Int` literal on a `Float` base is rounded to `Float`);
- **`BigInt`/`BigDecimal` bases** (rows 12, 13): "everything else as a `Double` literal" keeps `5.0`/`0.0`, and a
  `Double` literal on a `BigDecimal` base goes through a `Double` (harmless: refined does the same, `BigDecimal(d)`);
- **a non-whole literal on an integral base** (`Int Refined Less[1.5]`: refined can't validate it, no `WitnessAs`,
  but the type is legal and `Json.schema` accepts it): "as a `Long` literal" would truncate `1.5` to `1` and change the
  meaning (today `1.5`, right by accident).

So B31 is the refined half of B19: one exact literal model for both jobs, with an emission rule that reproduces B19
step 7 on every case B19 lists (`Int Refined Greater[17]` → `17`, `Double Refined Greater[1.5]` → `1.5`,
`Double Refined Positive` → `0.0`, `Long Refined Less[9007199254740993L]` exact, `Int Refined Divisible[20]` → `20`,
`fs-example` `16`/`512`) and covers the rest.

## Why it is a problem

- **Wrong schemas.** Rows 1, 4, 5, 6, 10: the schema accepts values refined rejects, or rejects values refined
  accepts. `Long` bounds near or beyond 2^53 are common for ids, timestamps in nanoseconds and `Long.MaxValue`-style
  caps. Row 2: `9.223372036854776E+18` is 2^63, outside `Long`.
- **Missing compile error.** Row 7: an empty range compiles to an `allOf` nothing satisfies, while the same range with
  small numbers is a compile error (documented in `docs/src/main/paradox/modules/refined.md`).
- **Inconsistent text.** Rows 11–13: integer bounds on `integer`/`BigInt` schemas print `.0`; the README and the
  paradox refined page already promise `{"type": "integer", "exclusiveMinimum": 17}`. Error messages print `2.0` for
  `Int` literals.
- Who hits it: refined users with `Long` bounds (wrong values), everybody with integral refined bounds (the `.0`).

## Options

### A. An exact literal model (`Lit`) for emission, comparison and messages — WON
Both macros replace a predicate's `v: Any` (and the `vv: Double` derived from it) with a `Lit`, built once per literal
from the refined base type `t` and the literal value:

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
`internal/WitnessAs.scala`: a literal of the base type itself; `Int` → `Byte`/`Short`/`Long`/`BigInt`/`Float`/`Double`/
`BigDecimal`; `Double` → `Float`/`BigDecimal`), held exactly:

| base type `t` | kind | value |
|---|---|---|
| `Byte`, `Short`, `Int`, `Long`, `BigInt`, `java.lang.{Byte,Short,Integer,Long}`, `java.math.BigInteger` | `Integral` if the value is whole, else `Decimal` | `Int`/`Long` literal: exact. A `Double`/`Float` literal (no `WitnessAs`; legal type): its exact decimal value, so `Less[1.5]` stays `1.5` |
| `Float`, `java.lang.Float` | `Float32` | the literal `.toFloat` (refined's `intWitnessAsFloat`/`doubleWitnessAsFloat`), as the `Float`'s shortest decimal (`BigDecimal(f.toString)`) |
| `Double`, `java.lang.Double` | `Float64` | the literal `.toDouble` (exact for `Int`; a `Long` beyond 2^53 is rounded, as refined would if it accepted it), as `BigDecimal(d)` (the shortest decimal) |
| `BigDecimal`, `java.math.BigDecimal` | `Decimal` | `Int`/`Long` exact; `Double` as `BigDecimal(d)` (refined's `doubleWitnessAsBigDecimal`); `Float` as its shortest decimal |
| anything else (a user type with a `Numeric`) | the literal's own: `Int`/`Long` → `Integral`, `Float` → `Float32`, `Double` → `Float64` | the literal, exact |

Shortest-decimal forms preserve order (distinct `Float`s/`Double`s get distinct, equally ordered decimals), so
comparing `value`s is exact for every kind.

**Emission** picks B19's typed `:=` overload from `kind`: `Integral` → a `Long` literal if the value fits a `Long`,
else `BigDecimal("<digits>")` (no literal can exceed a `Long` today; the branch is for completeness); `Float32` → a
`Float` literal; `Float64` → a `Double` literal; `Decimal` → `scala.math.BigDecimal("<digits>")`. B19 renders each
exactly (`17`, `9007199254740993`, `0.1`, `1.6777216E+7`, `0.0`, `0`).

**Normalisation**: `Inside`/`Outside` compare `Lit`s; `Ge.min/max`, `Le.min/max` take `if (a.v <= b.v) a.v else b.v`
(the `Lit` ordering) instead of `math.min`/`math.max`, which go away (Scala 2 `Math.scala` is deleted, Scala 3's
`private object math` too). The inclusiveness merge (`inclusive || o.inclusive`) and which pairs are matched stay as
they are: that logic is B27's. `Size`'s `asSize` takes `lit.value.toInt` (same as today's `vv.toInt` for every value
it can hold; B32 owns `Size`'s semantics).

**Messages**: `There are no values inside of specified range [${min.v.show} .. ${max.v.show}]`.

Output (Draft 07; JVM and JS; all profiles):

| # | type | today | A |
|---|---|---|---|
| 1 | `Long Refined Less[9007199254740993L]` | `9007199254740992` | `9007199254740993` |
| 2/3 | `Long Refined LessEqual[Long.MaxValue]` / `GreaterEqual[Long.MinValue]` | `±9.223372036854776E+18` | `9223372036854775807` / `-9223372036854775808` |
| 4 | `Long Refined Divisible[9007199254740993L]` | `9007199254740992` | `9007199254740993` |
| 5 | `Interval.Closed[..993, ..995]` | `..992`, `..996` | `..993`, `..995` |
| 6 | `Interval.Closed[..992, ..993]` | `allOf[{minimum ..992},{maximum ..992}]` | `{"type": "integer", "minimum": 9007199254740992, "maximum": 9007199254740993}` |
| 7 | `GreaterEqual[..993] And LessEqual[..992]` | compiles | `There are no values inside of specified range [9007199254740993 .. 9007199254740992]` |
| 8 | `GreaterEqual[5] And GreaterEqual[..993]` | `..992` | `..993` |
| 9 | `Float Refined Greater[0.1f]` | `0.10000000149011612` | `0.1` |
| 10 | `Float Refined Less[16777217]` | `16777217` | `1.6777216E+7` (B19's `Float` rendering of 16777216f) |
| 11 | `Int Refined Greater[17]` | `17.0` | `17` |
| 12 | `BigInt Refined Greater[5]` | `5.0` | `5` |
| 13 | `BigDecimal Refined Positive` | `0.0` | `0` |
| 14 | `Double Refined Positive` / `Greater[0.1]` / `Divisible[0.1]` | `0.0` / `0.1` / `0.1` | unchanged |
| — | `Int Refined Less[1.5]` (not validatable by refined) | `1.5` | `1.5` (B19 step 7 as written: `1`) |
| — | message for `Int Refined (Less[2] And Greater[7])` | `[2.0 .. 7.0]` | `[2 .. 7]` (a `Double` base keeps `[2.0 .. 7.0]`) |

Pros: every number is the one refined compares against, on both jobs, in one place per macro; both macros keep the
same structure (they are a port of each other); comparisons can't drift from emission. Cons: a slightly bigger diff
than B (every `NumericPred` field changes type), overlapping the code B27 rewrites (see Implementation plan, step 0).

### B. Emission only: B19 step 7 as approved — LOST
Keep `v: Any`/`vv: Double` for comparisons and messages; emit at the base type from the original literal
(B19 step 7). Fixes rows 1–4, 9, 11 and the emission half of 5 and 8. Leaves rows 6 and 7 (normalisation on
`Double`s: a wrong `allOf`, a missing compile error), row 10, rows 12/13 (`5.0`, `0.0` on `BigInt`/`BigDecimal`), the
`Double`-printing messages, and truncates `Int Refined Less[1.5]` to `1`. Smaller, but it keeps two notions of a
literal's value (one to compare, one to emit) that disagree exactly where B31 is about. Lost on rows 6/7.

### C. Exact comparisons on the raw literal, no conversion to the base type — LOST
Keep `v: Any`, but compare through `BigDecimal` (`toBig(v: Any)` in `Inside`/`Outside`, `math.min/max`'s `Number`
branch) and emit with B19 step 7. Fixes everything A fixes except row 10 (an `Int` literal on a `Float` base stays
`16777217`, which accepts `16777216f`, refined rejects it) and rows 12/13 unless step 7's rule is extended anyway. It
is A without the `Lit` type: the conversion and kind logic would be spread over `Inside`, `Outside`, `math` and the
emitters instead of one constructor. No real saving over A; lost.

### D. Reject literals a `Double` can't hold — LOST
A compile error for a `Long` literal beyond 2^53 ("refined bound 9007199254740993 can't be represented"). Honest, tiny,
but refined accepts these types and users have them (ids, `Long.MaxValue` caps): a regression from "slightly wrong"
to "doesn't compile". Lost.

## Decision

A: both refined macros build one exact `Lit` per numeric literal (the value refined's `WitnessAs` gives on the base
type, plus how the base type prints it) and use it to compare, normalise, report and emit; emission goes through B19's
typed `:=` overloads. It reproduces B19 step 7 on every case B19 lists and replaces step 7's rule where step 7 is
lossy or changes the meaning.

## Implementation plan

0. **Order and overlaps.**
   - **Depends on B19 (#512)**: the core part (P1-D typed `:=` overloads, P1-A exact `num(x: Number)`, P2
     `Decimals.fromFloat`). Without it a `Long` literal goes through `:=(Number)` and `doubleValue` again. B31 lands in
     the same release as B19, after it or in the same PR. B31 **replaces B19's step 7**: if B19 is implemented first,
     its step 7 can be done as this doc's steps 2–4 directly (same output for every case B19 lists), or done as written
     and then rewritten here; the B19 implementer and the lead choose. Either way the refined golden ends at `16`/`512`.
   - **B27 (refined `Or` normalisation)** rewrites `Or`/`And` compile, `Inside`/`Outside` and the inclusiveness merge in
     the same two files. B31 only changes the *type* of the compared values (`vv` → `Lit`) and removes `math`. Whichever
     PR lands second rebases (conflicts are local to those functions); if B27 is first, its comparisons switch to `Lit`'s
     ordering. **B32** (`Size` exclusive bounds ±1) adjusts `asSize`, which reads `lit.value` after B31 (exact `+1`/`-1`).
     **D2** (`Short`/`Byte`/`BigInt` → `integer`) changes `isInt`/`prefix`; independent of `Lit.kind`.
1. **Scala 2 `modules/refined/src/main/scala-2/com/github/andyglow/jsonschema/refined/`**
   - new `Literals.scala`: `private[jsonschema] trait Literals { this: HasContext with HasLog => ... }` with `Lit`,
     `Lit.Kind`, `Lit.apply(t: Type, v: Any)` (the table in A; base types matched with `=:=` against `typeOf[...]`,
     after `t.dealias`), `Lit.show`, and `def litTree(l: Lit): Tree` (`Integral` → `Literal(Constant(l.value.toLong))`
     when `l.value.isValidLong`, else `q"_root_.scala.math.BigDecimal(${l.value.toString})"`; `Float32` →
     `Literal(Constant(l.value.toFloat))`; `Float64` → `Literal(Constant(l.value.toDouble))`; `Decimal` →
     `q"_root_.scala.math.BigDecimal(${l.value.bigDecimal.toPlainString})"`). A non-`Number` literal: `err(s"refined: $v is not a number")`
     (Scala 3's `asNumber` message; Scala 2 today casts with `asInstanceOf[Number]` inside the macro; both versions
     already fail to compile `Char Refined Greater['a']`, so only the message can change).
   - `Logic.scala`: the cake `extends Extractors with HasLog with AST with Literals with HasContext` (drop `Math`).
   - `AST.scala`: `NumericPred.v: Lit` (drop `vv`); `Divisable(t, v: Lit)`, `Ge(t, v: Lit, ...)`, `Le(t, v: Lit, ...)`;
     `Pos/NonPos/Neg/NonNeg` use `Lit(t, 0)`; every `:= $vv` / `:= ${min.vv}` becomes `:= ${litTree(..)}`;
     `asSize` = `Size.Def.Min(v.value.toInt)` / `Max(...)`; `min`/`max` compare `Lit`s; `Inside`/`Outside` compare
     `l.v < r.v` etc.; the `And` error uses `min.v.show`/`max.v.show`. `AST`'s self-type gains `Literals`, loses `Math`.
   - `Extractors.scala`: `Ge(t, Lit(t, v))` etc. for `sGreater`, `sGreaterEqual`, `sLess`, `sLessEqual`,
     `sDivisable`, `sNonDivisable`.
   - delete `Math.scala`.
2. **Scala 3 `modules/refined/src/main/scala-3/com/github/andyglow/jsonschema/refined/RefinedMacro.scala`**
   - `Lit` as a private case class of `RefinedMacro` (needs `q.reflect` for the base-type test: `t =:= TypeRepr.of[Long]`
     etc. on `t.dealias`); `Lit(t: TypeRepr, v: Any)` replaces `asNumber`.
   - `num(v: Double): Expr[Number]` goes; `minimum(l: Lit, inclusive)`, `maximum(l: Lit, inclusive)`,
     `multipleOf(l: Lit)` match on `l.kind` and splice a typed literal so the call resolves to B19's overload:
     `'{ Instance.`minimum` := ${Expr(l.value.toLong)} }` (`Long`), `${Expr(l.value.toFloat)}`, `${Expr(l.value.toDouble)}`,
     `'{ Instance.`minimum` := scala.math.BigDecimal(${Expr(s)}) }`. Their result type stays `Expr[Def[Number, ?]]`.
   - `NumericPred.v: Lit`, `Ge`/`Le`/`Divisable` fields, `Pos`/`Neg`, `asSize`, `min`/`max`, `Inside`/`Outside`, the `And`
     error: as in step 1. Delete `private object math`.
   - the class scaladoc: replace "Bounds and `multipleOf` are emitted as Doubles" with "a numeric literal is read as
     refined converts it to the base type and emitted exactly at that type".
3. **Tests**, refined module (shared dirs run on JVM and JS):
   - new `modules/refined/src/test/scala-2.13+/com.github.andyglow.jsonschema.refined/RefinedPrecisionSpec.scala`
     (literal types: 2.13 and 3): every row of A's output table as rendered Draft 07 text (`stringify` + a
     whitespace-normalised `include`, as in the repro), including row 6's full object and row 14 unchanged, plus
     `Int Refined Less[1.5]` → `1.5` and `Short Refined Greater[5]` → `5`.
   - new `modules/refined/src/test/scala-2.12-/com.github.andyglow.jsonschema.refined/RefinedPrecisionSpec.scala`: the
     same rows with ``W.`...`.T`` literals (rows 1, 2, 5, 6, 9, 10, 11, 12 at least).
   - empty range by precision (row 7) and the message format: Scala 3 `Scala3RefinedErrorsSpec`: update the three
     `[2.0 .. 7.0]` expectations to `[2 .. 7]`, add row 7's message and a `Double` base (`Double Refined (Less[2] And
     Greater[7])` → `[2.0 .. 7.0]`). Scala 2: new `modules/refined/src/test/scala-2/com.github.andyglow.jsonschema.refined/Scala2RefinedErrorsSpec.scala`
     with `shapeless.test.illTyped` (shapeless is on refined's Scala 2 classpath; ``W.`...`.T`` literals so one file
     serves 2.12 and 2.13) for the same three messages. Scala.js: these are compile-time checks; they run on JS too.
   - existing tree specs (`RefinedNumbersSpec`, `RefinedBooleanSpec`, `RefinedQuirksSpec`, `RefinedAliasedSpec`,
     `RefinedFieldsSpec`): no change expected (`Def` equality compares the `Value`, and `num` equality is numeric). If
     one fails, the failure is a real output change: report it.
4. **Golden files**: `modules/refined/src/test/resources/refined/fs-example.draft07.json`: `16.0`/`512.0` → `16`/`512`
   (`Int` base; the same change B19 step 7 makes: whichever lands first does it). No `api` golden or `golden-legacy`
   file uses refined.
5. **Docs** (README and paradox say the same): README "Refined" section (after the supported-predicates list) and
   `docs/src/main/paradox/modules/refined.md` (before "An unsupported predicate..."): add
   "Numeric bounds keep the literal's exact value, as refined reads it for the base type: `Long Refined
   Less[9007199254740993L]` gives `"exclusiveMaximum": 9007199254740993`, `Int` bounds print as integers (`17`), `Double`
   bounds as `Double`s (`Double Refined Positive` gives `0.0`)." No CHANGELOG edit (the lead's).
6. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9 (`clean`: the macros change); `rootJS/test`
   on 2.13.18 and 3.3.9 (`refinedJS` at least); `scalafmtCheckAll` (+ `rootJS/scalafmtCheckAll`); `makeDocs` (README and
   paradox change).

Scala 2 vs 3: the same `Lit` table in both, tested by the shared `scala-2.13+` spec on 2.13 and 3 and by the shared
golden. Scala 2 emits `Literal(Constant(...))` trees, Scala 3 `Expr(...)`; both make the static type of the right-hand
side `Long`/`Float`/`Double`/`BigDecimal`, which is what selects B19's overload. Scala.js: the macros run on the JVM at
compile time (`BigDecimal` there is fine); the emitted `Long` literal is a `RuntimeLong` and B19's `:=(Long)` keeps it
exact; `Float` literals render through B19's JS `Decimals.fromFloat`.

Risks: the `Float`-base conversion (row 10) changes a bound's value (16777217 → 16777216), deliberately, to match
refined; `-Xcheck-macros` on Scala 3 (spliced literals are plain constants, low risk); a user predicate alias that hides a
base type (`type Id = Long`): `Lit.apply` dealiases `t`, as `isInt` should (today `isInt` uses `=:=`, which already
sees through aliases).

## Compatibility

- Source/binary: no public API change (the macros and their helpers are `private[jsonschema]`). The code the macros
  emit at user call sites changes and needs B19's typed `:=` overloads (same release).
- Output (all profiles, Legacy included; JVM and Scala.js): as A's table. `Long` bounds exact; integral bases print
  integers (`17`, also from B19 step 7); `BigInt`/`BigDecimal` bases print whole bounds as integers (`5`, `0`);
  `Float` bases print the `Float` refined compares against (`0.1`, `1.6777216E+7`); `Double` bases unchanged; row 6 a
  proper interval instead of an `allOf`. Golden: `refined/fs-example.draft07.json` only (shared with B19). No
  `golden-legacy` change. Under the Legacy policy these are value corrections (rows 1–8, 10) and B19's number layout
  (rows 11–13), not a style change; keeping Legacy's `17.0` would be the alternative (not proposed: B19 doesn't either).
- Compile errors: row 7 newly fails to compile (it accepted no value anyway); the empty-range message prints `[2 .. 7]`
  instead of `[2.0 .. 7.0]` for integral bases.
- PVP: no API change; a patch by the API rule, riding B19's release (the lead decides whether the text changes count as
  breaking, as for B19).

## Review log
