# Numbers keep their exact value on the way to JSON  (issue #512, item B19)
Status: approved (r-b19, 2026-11-23)

## Problem

A Scala number becomes a JSON value (`com.github.andyglow.json.Value.num`, which holds a `BigDecimal`) on five paths.
Four of them go through `Double` and lose digits; the fifth renders whole numbers as `5.0`:

| # | path | code | what it does |
|---|---|---|---|
| P1 | validation bounds (`minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`, `multipleOf`) and `ToValue[Number]` | `Value.num.apply(x: Number)` (Value.scala:89), used by `Instance` (validation/Instance.scala:17-25) | `Decimals.fromDouble(x.doubleValue())` |
| P2 | `Float` values (field defaults through `ToValue.FloatV`, `Value.num(0.1f)`) | `Value.num.apply(x: Float)` (Value.scala:82) | `Decimals.fromDouble(x.toDouble)` |
| P3 | the implicit conversion to `Value` (`val v: Value = 1L`, `obj("a" -> 5L)`, `ToValue mk { case A => 1L }`) | `ValueAdapter.numberAdapter` (Value.scala:298) | `num(x)` for `Int`, `num(n.toDouble(x))` for every other `Numeric` |
| P4 | circe defaults/enum values (`import AsCirce._`) | `AsCirce.toValue` (AsCirce.scala:36) | `num(x.asNumber.get.toDouble)` |
| P5 | upickle defaults/enum values (`import AsU._`) | `AsU.Adapter.numAdapter` (AsU.scala:71) | `ujson.Num` holds a `Double`: whole numbers come back as `5.0` |

Repro (specs and logs in `notes/scratch/c-b19/`, not committed). Draft 07, the same on 2.12.21, 2.13.18 and 3.3.9 (JVM),
and on 2.13.18 and 3.3.9 Scala.js (Bun):

| input | today | expected |
|---|---|---|
| `Json.schema[Long].withValidation(maximum := Long.MaxValue)` | `"maximum": 9.223372036854776E+18` (that is 2^63, above `Long.MaxValue`) | `9223372036854775807` |
| `... maximum := 5L` | `5.0` | `5` |
| `Json.schema[Int].withValidation(maximum := 20, minimum := 0)` | `20.0`, `0.0` | `20`, `0` |
| `... maximum := BigDecimal("12345678901234567890.123")` | `1.2345678901234567E+19` | `12345678901234567890.123` |
| `... maximum := BigInt("123456789012345678901234567890")` | `1.2345678901234568E+29` | `123456789012345678901234567890` |
| `... maximum := 0.1f` | `0.10000000149011612` | `0.1` |
| `... maximum := 1.5` | `1.5` | `1.5` (unchanged) |
| `case class F(f: Float = 0.1f)` | `"default": 0.10000000149011612` | `0.1` |
| `case class F(f: Float = 16f, d: Double = 16.0, d2: Double = 0.1)` | `16.0`, `16.0`, `0.1` | unchanged |
| `case class I(l: Long = Long.MaxValue, s: Long = 5L, bi: BigInt = ..., bd: BigDecimal = ...)` (core `ToValue`) | exact (`9223372036854775807`, `5`, ...) | unchanged |
| `@typeHint[Long]` family, `ToValue mk { case A => 1L; case B => Long.MaxValue }` | `"enum": [1.0, 9.223372036854776E+18]` | `[1, 9223372036854775807]` |
| `val v: Value = 1L` / `Long.MaxValue` / `BigDecimal("12345678901234567890.123")` / `0.1f` | `1.0` / `9.223372036854776E+18` / `1.2345678901234567E+19` / `0.10000000149011612` | `1` / `9223372036854775807` / `12345678901234567890.123` / `0.1` |

The bridges' `ToValue` (defaults of `case class D(l: Long = Long.MaxValue, s: Long = 5L, bi: BigInt = BigInt("1234...890"),
bd: BigDecimal = BigDecimal("12345678901234567890.123"), f: Float = 0.1f, d: Double = 16.0, d2: Double = 0.1)`), compared
with what each library itself writes for those values:

| bridge | schema `default`s today | the library writes | verdict |
|---|---|---|---|
| circe | `l` 9.223372036854776E+18, `s` 5.0, `bi` 1.2345678901234568E+29, `bd` 1.2345678901234567E+19, `f` 0.1, `d` 16.0 | `9223372036854775807`, `5`, `1234...890`, `12345678901234567890.123`, `0.1`, `16.0` | **P4: lossy** |
| upickle | `l` "9223372036854775807", `s` 5.0, `bi` "1234...890", `bd` "12345678901234567890.123", `f` 0.1, `d` 16.0 | `"9223372036854775807"`, `5`, `"1234...890"`, `"12345678901234567890.123"`, `0.1`, `16` | values exact (big numbers are strings in upickle); **P5: `5.0` for `5`** |
| play-json | exact (`JsNumber` holds a `BigDecimal`) | same values (`bi` as `1.2345678901234567890123456789E+29`) | fine |
| spray-json | exact | same | fine |
| json4s | `f` 0.10000000149011612, `bd` 1.2345678901234567E+19, the rest exact | `JDouble(0.10000000149011612)`, `JDouble(1.2345678901234567E19)` (json4s' `DefaultWriters`) | fine: the schema shows what json4s writes |

On Scala.js the libraries write some numbers differently, because Scala.js' `Double.toString(16.0)` is `16` and
`Float.toString(0.1f)` is `0.10000000149011612`: circe and upickle write `16` and `0.10000000149011612`, play writes `16`.

Rendering *into* a library AST: play, spray, json4s (`JDecimal`) and circe (`Json.fromBigDecimal`) keep the value.
`asU` goes through `ujson.Num(x.value.toDouble)` (AsU.scala:71): `ujson.Value` has no exact number, so a bound or default
beyond 2^53 or with more than 17 significant digits loses precision there.

## Why it is a problem

- **Validation failures.** An `enum`/`const` built from a rounded value rejects the real value. networknt 1.5.9
  (Draft 04, 07, 2020-12, probe `B19ValidatorProbeSpec`): `{"enum": [9.223372036854776E+18]}` rejects `9223372036854775807`
  (what circe and play write for `Long.MaxValue`) with "does not have a value in the enumeration";
  `{"enum": [0.10000000149011612]}` rejects `0.1` (what the JVM libraries write for `0.1f`; reachable through `ToValue`
  enums and E6's `Float` literal types).
- **Wrong bounds.** `maximum := Long.MaxValue` renders 2^63, so the schema accepts `9223372036854775808`, which no
  `Long` holds (networknt: valid). A 20-digit `BigDecimal` bound becomes a different number.
- **Wrong defaults.** `default` is an annotation, but tools show it and generate code and forms from it:
  `0.10000000149011612`, `1.2345678901234567E+19` are not the values in the source.
- **Inconsistent layout.** `maximum := 20` on an `integer` schema renders `20.0`, `ToValue mk { case A => 1L }` renders
  `1.0`, while `Value.num(5L)` and the core `ToValue[Long]` render `5`. D2 (#491) moves `Long` to `integer`, which makes
  `"type": "integer", "enum": [1.0]` look contradictory (r-d2's D2-R6).

Who hits it: anyone with `Long`/`BigInt`/`BigDecimal` bounds or enum values, `Float` defaults, circe users with big
numeric defaults; everybody with an integer bound sees the `.0`.

## Options

### P1: validation bounds (`Value.num(x: Number)`)

The `:=` of the numeric `Instance`s takes a `java.lang.Number`: `maximum := 20` boxes through `Predef.int2Integer`, so
`num` only sees the boxed value. On Scala.js a boxed `Int`, `Short`, `Byte`, `Float` and `Double` are all the same JS
number. Probe (`B19PlatformProbeSpec`, 2.12 and 3.3.9 on Bun): a boxed `16` and a boxed `16.0` both pass
`isInstanceOf[Integer]`, `[Double]` and `[Float]`; a boxed `0.1f` passes `[Float]` and `[Double]`; a boxed `0.1` passes
only `[Double]`; a boxed `5L` passes only `[Long]`. So only `java.lang.Long` (a `RuntimeLong`), `BigInt`, `BigDecimal`
and `java.math.*` keep their class; `isInstanceOf[Float]` means "the value is exactly a `Float`".

#### D. Typed `:=` overloads on the numeric keywords — WON (from B19-R1)
Put the five numeric keywords under one intermediate class and give it an overload per numeric type, next to the
inherited `:=(x: Number)`:

```scala
sealed abstract class NumericInstance extends Instance[Number, Number]()(num.apply) {
  def :=(x: Int): Def[Number, Number]        = Def(this, Int.box(x), num(x))
  def :=(x: Long): Def[Number, Number]       = Def(this, Long.box(x), num(x))
  def :=(x: Float): Def[Number, Number]      = Def(this, Float.box(x), num(x))
  def :=(x: Double): Def[Number, Number]     = Def(this, Double.box(x), num(x))
  def :=(x: BigInt): Def[Number, Number]     = Def(this, x, num(x))
  def :=(x: BigDecimal): Def[Number, Number] = Def(this, x, num(x))
}
case object `maximum` extends NumericInstance   // likewise multipleOf, minimum, exclusiveMaximum, exclusiveMinimum
```

The static type picks the overload at compile time, so each bound renders through the typed `num` overload (P2 for
`Float`), the same on the JVM and Scala.js. r-b19's probe (`notes/scratch/r-b19/OverloadProbe.scala`, 2.12.21, 2.13.18,
3.3.9): `:= 20` → Int, `5L` → Long, `16.0` → Double, `0.1f` → Float, `Short`/`Byte` → Int (widening), `BigInt`/`BigDecimal`
→ their own overload, a `java.lang.Long` or a `Number` value → the inherited `Number` overload; no ambiguity, no warning.
Output: `maximum := 20` → `20`, `5L` → `5`, `Long.MaxValue` → `9223372036854775807`, `16.0` → `16.0`, `1.5` → `1.5`,
`0.1f` → `0.1`, `BigDecimal`/`BigInt` exact. `Def`, `Instance[S, V]`, `withValidation` and the `Magnet`s don't change;
`Def.value` is still the boxed `Number`.
The inherited `:=(x: Number)` (boxed values, refined's Scala 3 macro today, `ToValue[Number]` through the same
`num(x: Number)`) keeps the value exact with the rule of A below, because there the boxed class is unreliable on Scala.js.
Pros: exact values, each bound printed the way its type prints (so the README/getting-started sentence "a `Double` bound
of `16.0` is `16.0`" holds as written), no signature change. Cons: refined's macros must pass their bounds at a static
type, consistently on Scala 2 and 3 (step 7), or `fs-example.draft07.json` would differ between them; `:= 'c'` starts to
compile (a `Char` widens to `Int`, renders `99`), harmless; the five case objects get a new superclass and six methods
(binary-compatible additions).

#### A. Exact value, whole numbers as integers — LOST as the whole answer; kept for the `Number` overload
`num(x: Number)` dispatches on the class where the class is reliable on both platforms, and on the value otherwise:

```scala
def apply(x: Number): num = new num(x match {
  case x: scala.math.BigDecimal => x
  case x: scala.math.BigInt     => BigDecimal(x)
  case x: java.math.BigDecimal  => BigDecimal(x)
  case x: java.math.BigInteger  => BigDecimal(BigInt(x))
  case x: java.lang.Long        => BigDecimal(x.longValue)
  case x                        =>   // Integer, Short, Byte, Float, Double; other Numbers (AtomicLong, LongAdder): doubleValue, out of scope
    val d = x.doubleValue
    if (d.isWhole && d > -9.2e18 && d < 9.2e18) BigDecimal(d.toLong)  // 20 -> 20, 16.0 -> 16, 1e18 -> 1000000000000000000
    else if (x.isInstanceOf[java.lang.Float]) Decimals.fromFloat(x.floatValue)
    else Decimals.fromDouble(d)                                     // 1.5, 1.0E+300; NaN/Infinity still throw
})
```

Alone (every bound through this method, as today's single `:=`): `maximum := 20` → `20`, `5L` → `5`, `Long.MaxValue`
→ `9223372036854775807`, `BigDecimal`/`BigInt` exact, `0.1f` → `0.1`, `1.5` → `1.5`, `16.0` → `16`. Identical on the JVM
and on Scala.js. Pros: one method, no new members. Cons: a whole `Double` bound (`maximum := 16.0`) renders `16`, which
contradicts README:77 / getting-started.md:38 ("a `Double` bound of `16.0` is `16.0`"); D has the same exactness without
that. Lost as the whole answer for that reason; with D it only serves the `Number` overload, where `16` for a boxed
whole value is the best that works on both platforms. Scala.js corner (`Number` overload only): a boxed `Double` whose
value is exactly a `Float` *and* needs more than 9 digits (e.g. `0.10000000149011612`) renders as that `Float`'s digits
(`0.1`) there; nobody writes such a value on purpose.

#### B. Type-based dispatch on the boxed class — LOST
In `num(x: Number)` (no overloads): `Integer`/`Short`/`Byte` → integer, `Double` → `Decimals.fromDouble`, `Float` →
`fromFloat`, the rest as in A.
On the JVM it keeps `maximum := 16.0` as `16.0`. On Scala.js the class tests can't be told apart (a whole `Double`
passes `isInstanceOf[Integer]`), so `maximum := 16.0` renders `16.0` on the JVM and `16` on JS: it breaks the
JVM/JS parity the docs promise and the refined golden (`fs-example.draft07.json`, run on both platforms; refined passes
its bounds as `Double`s). Lost on that.

#### C. Static types: a `Bound` number for the numeric keywords — LOST
Make the numeric `Instance`s `Instance[Number, Bound]` with `final class Bound(val number: Number, val value: Value)
extends java.lang.Number` and implicit conversions in `object Bound` (`fromInt`, `fromLong`, `fromDouble`, `fromFloat`,
`fromBigInt`, `fromBigDecimal`, `fromNumber`), so `maximum := 16.0` picks `fromDouble` at compile time and renders
`16.0`, `maximum := 20` picks `fromInt` and renders `20`, on both platforms.
Pros: the most precise layout. Cons: a public signature change (`Def[Number, Number]` → `Def[Number, Bound]`: breaking
for code that names the type or builds a `Def` by hand; refined's macros need updating), relies on implicit-view
specificity among numeric widenings (`Int` also widens to `Long`/`Double`) that differs between Scala 2 and 3 and needs a
probe. D gives the same static-type layout with plain overloads and no signature change, so C has nothing left to offer.

### P2: `Float` values (`Value.num(x: Float)`)

#### A. Shortest decimal that round-trips to the `Float` — WON
New `Decimals.fromFloat(f: Float): BigDecimal` next to `fromDouble`, with Java's layout (`0.1`, `16.0`, `1.0E+10`):
- JVM (`scala-jvm/.../Decimals.scala`): `BigDecimal(java.lang.Float.toString(f))`. JDK 19+ prints the shortest digits
  (the build and CI use 21); JDK 8–18 print a longer but still round-tripping string for a few values, the same caveat
  `fromDouble` already has with `Double.toString`.
- Scala.js (`scala-js/.../Decimals.scala`): Scala.js' `Float.toString` gives the double's digits
  (`0.10000000149011612`), so search: the smallest `p` in 1..9 with `d.toPrecision(p).toDouble.toFloat == f`
  (`d = f.toDouble`; the `js.Math` facade has no `fround`, and `Double.toFloat` is `fround` on Scala.js), then lay the
  digits out with the existing `fromDouble` layout code (factor it into a private `layout(...)`), including its
  two-digit rule for one-digit results (`Float.MinPositiveValue`: the search gives `1e-45`, Java prints `1.4E-45`).
  Probe (`B19JsFloatProbeSpec`, Bun): `0.1f` → `0.1`, `1.1f` → `1.1`, `Float.MaxValue` → `3.4028235e+38`,
  `1.0e-4f` → `0.0001`, `123456.79f` → `123456.79`, `0.3f` → `0.3`, `2.5e-8f` → `2.5e-8`.
- `Value.num(x: Float) = new num(Decimals.fromFloat(x))`.

Output: `0.1f` → `0.1`, `1.1f` → `1.1` (today `1.100000023841858`), `16f` → `16.0` (unchanged), `1e10f` → `1.0E+10`.
Same on both platforms. This matches what circe, play and spray write on the JVM. On Scala.js those libraries
themselves write `0.10000000149011612`, so a core `ToValue[Float]` default (`0.1`) and the library's JS output differ
there; that is the libraries' JS behaviour, and the bridges' `ToValue` (P4) shows what the library writes.

#### B. Keep `toDouble` — LOST
`0.10000000149011612` is not the value in the source and an `enum`/`const` with it rejects the libraries' `0.1`.

### P3: the implicit conversion (`ValueAdapter.numberAdapter`)

#### A. Dispatch on the `Numeric` instance — WON
The `Numeric[T]` evidence knows the static type, on both platforms:

```scala
def adapt(x: T): num = n match {
  case _: Numeric.IntIsIntegral          => num(x.asInstanceOf[Int])
  case _: Numeric.LongIsIntegral         => num(x.asInstanceOf[Long])
  case _: Numeric.ShortIsIntegral        => num(x.asInstanceOf[Short])
  case _: Numeric.ByteIsIntegral         => num(x.asInstanceOf[Byte])
  case _: Numeric.FloatIsFractional      => num(x.asInstanceOf[Float])
  case _: Numeric.DoubleIsFractional     => num(x.asInstanceOf[Double])
  case _: Numeric.BigIntIsIntegral       => num(x.asInstanceOf[BigInt])
  case _: Numeric.BigDecimalIsConflicted => num(x.asInstanceOf[BigDecimal])  // BigDecimalIsFractional and BigDecimalAsIfIntegral
  case _ =>                                                                 // other Numerics (Char, user-defined, 2.12's Double/FloatAsIfIntegral)
    x match {                                                               // classes that are reliable on Scala.js too
      case b: BigDecimal     => num(b)
      case b: BigInt         => num(b)
      case l: java.lang.Long => num(l.longValue)
      case _                 => num(n.toDouble(x))                          // as today
    }
}
```

The traits exist under these names in 2.12 and 2.13 (Scala 3 uses the 2.13 library); `BigDecimalIsConflicted` is the
parent of both `BigDecimal` instances in both (r-b19 checked the jars). The probe (`B19PlatformProbeSpec`) matches the
other seven, and `Char` falls through, on 2.12 and 3.3.9, JVM and JS. Output: `1L` → `1`,
`Long.MaxValue` exact, `BigDecimal`/`BigInt` exact, `0.1f` → `0.1`, `16.0` → `16.0`, `5.toShort` → `5` (today `5.0`).

#### B. Dispatch on the runtime class of `x` — LOST
Same Scala.js problem as P1-B: a boxed `16.0` is an `Integer` there.

#### C. One implicit `ValueAdapter` per numeric type — LOST
Equivalent output, but eight more implicits next to the generic `numberAdapter` need a priority layer to stay
unambiguous on Scala 3; A is one method.

### P4: circe (`AsCirce.toValue`)

#### A. `JsonNumber.toBigDecimal` — WON
`case x if x.isNumber => val n = x.asNumber.get; num(n.toBigDecimal.getOrElse(Decimals.fromDouble(n.toDouble)))`.
circe 0.14.17's `toBigDecimal` is exact for every kind it builds: `JsonLong` → `BigDecimal(value)`, `JsonBigDecimal` →
the value, `JsonDouble` → `java.math.BigDecimal.valueOf(d)` (= `Double.toString`), `JsonFloat` →
`new BigDecimal(Float.toString(f))`, parsed numbers → the input text. It is `None` only for a parsed number whose
exponent doesn't fit an `Int`; the fallback keeps today's behaviour for those (`toDouble`, which throws for an
infinity as it does now).
Output on the JVM: exactly circe's text (`9223372036854775807`, `5`, `1234...890`, `12345678901234567890.123`, `0.1`,
`16.0`). On Scala.js the `Double`/`Float` kinds follow circe's JS output (`16`, `0.10000000149011612`), as play's
bridge already does today.

#### B. `BigDecimal(n.toString)` — LOST
The same values (`toString` is what circe prints), but it re-parses text and has no better answer for the huge-exponent
case. A is circe's own API for this.

#### C. Match circe's number kinds to keep `16.0` on Scala.js — LOST
`JsonLong`, `JsonDouble`, ... are `private[circe]`; reaching them needs a helper compiled into `package io.circe`.
That ties the bridge to circe internals to restore a layout circe itself doesn't write on Scala.js.

### P5: upickle (`AsU`)

#### A. Whole `ujson.Num`s as integers in `toValue` — WON
`numAdapter`'s `unadapt`: `d.isWhole && math.abs(d) < 9.2e18` → `num(BigDecimal(d.toLong))`, else `num(d)` as today.
upickle writes whole doubles without a fraction (`16.0` → `16`, `5L` → `5`), so the schema now shows upickle's text
on both platforms. Values were already exact: upickle turns `Long`s beyond ±2^53 and every `BigInt`/`BigDecimal` into
strings before `ujson.Value`. The render direction (`asU`) stays as it is: `ujson.Num` is a `Double`; document it.

#### B. Build `Value` with a `upickle.core.Visitor` instead of `writeJs` — LOST
Sees `visitInt64`/`visitFloat64` separately, but upickle's own output writes `16.0` as `16` anyway, so it would show
`16.0` where upickle writes `16`. More code for a worse match.

### Legacy

The Legacy profile shapes trees; these values are computed before any profile is involved (`Def` holds its
`Value` from `:=`, defaults from `ToValue`). Per the lead's Legacy policy these are value fixes and apply under Legacy
too. The layout part (`maximum := 20` → `20` instead of `20.0`, an `Int` printed as an `Int`) is the same JSON number.
No `golden-legacy` file contains such a number (checked with `grep`; r-b19 checked too), so no Legacy golden changes.
Alternative: Legacy keeps `20.0` for `Int` bounds, which needs the renderer to re-render `Def.value` under the profile
(`AsDraftSupport`); not worth it for a layout no validator distinguishes, but listed for the lead.

## Decision

One rule: a number keeps its exact value and renders the way its Scala type prints it, the same on the JVM and on
Scala.js. Bounds get typed `:=` overloads (P1-D), so the static type decides; only a bound passed as a boxed `Number`
keeps the exact value and renders a whole value as an integer (P1-A), because the boxed class is unreliable on Scala.js.
refined's macros pass their bounds at the refined base type (step 7). `Float`s render as their shortest round-tripping
decimal (P2-A), the implicit conversion follows the `Numeric`'s type (P3-A), and the circe and upickle bridges show the
number exactly as the library writes it (P4-A, P5-A). play, spray and json4s already do.

## Implementation plan

1. **`core/src/main/scala-jvm/com/github/andyglow/json/Decimals.scala`**: add
   `def fromFloat(f: Float): BigDecimal = BigDecimal(java.lang.Float.toString(f))` (NaN/infinity throw, as `fromDouble`).
2. **`core/src/main/scala-js/com/github/andyglow/json/Decimals.scala`**: factor the layout part of `fromDouble` into
   `private def layout(digits: String, exp: Int, negative: Boolean): BigDecimal`; add `fromFloat(f)`: NaN/infinity →
   `BigDecimal(f.toDouble)` (throws), `0` → `BigDecimal(BigInt(0), 1)`, else the smallest `p` in 1..9 with
   `d.toPrecision(p).toDouble.toFloat == f` (`d = f.toDouble`; `toFloat` is `fround` on Scala.js, the `js.Math`
   facade has none), take its digits/exponent, `layout(...)` (with the two-digit rule). Keep the scaladoc style of the
   file (why JS differs).
3. **`core/src/main/scala/com/github/andyglow/json/Value.scala`**:
   - `num.apply(x: Float) = new num(Decimals.fromFloat(x))`;
   - `num.apply(x: Number)`: the dispatch in P1-A, with a scaladoc saying why whole values become integers (the boxed
     class is unreliable on Scala.js) and that it serves boxed values (`:=(x: Number)`, `ToValue[Number]`);
   - `ValueAdapter.numberAdapter`: the `Numeric` match in P3-A.
4. **`core/src/main/scala/json/schema/validation/Instance.scala`**: `sealed abstract class NumericInstance` with the
   six overloads of P1-D (each `Def(this, <boxed x>, num(x))`); `multipleOf`, `maximum`, `minimum`, `exclusiveMaximum`,
   `exclusiveMinimum` extend it. The `Number` overload is the inherited `:=`. Scaladoc: the overloads keep the static type
   because a boxed `Number` can't tell `Int` from `Double` on Scala.js.
5. **`modules/circe-json/src/main/scala/com/github/andyglow/jsonschema/AsCirce.scala`**: P4-A in `toValue`
   (`Decimals` is `private[andyglow]`, reachable from `com.github.andyglow.jsonschema`).
6. **`modules/u-json/src/main/scala/com/github/andyglow/jsonschema/AsU.scala`**: P5-A in `numAdapter`'s `unadapt`.
7. Superseded by B31 (#532): implement B31 as this step.

   **refined, both macros** (needed by P1-D: today Scala 2 emits `:= <Double literal>` and Scala 3 emits
   `:= java.lang.Double.valueOf(..)`, which would now pick the `Double` and the `Number` overloads and render `16.0` vs
   `16`). Emit every bound at the refined base type: integral bases (`Int`, `Long`, `Short`, `Byte` and boxed: the
   `isInt`/`integer` cases) as a `Long` literal, `Float` as a `Float` literal, everything else as a `Double` literal,
   taken from the predicate's original value (not from the `Double` used for comparisons), so a `Long` bound beyond
   2^53 stays exact.
   - Scala 2 `modules/refined/src/main/scala-2/.../AST.scala`: `Divisable`, `Bounded`, `Ge`, `Le` lift a
     `def lit: Tree` (by `t`) instead of `vv`; `vv` stays for `Size`/interval checks. `Math.min/max` already keep the
     original `Number`.
   - Scala 3 `modules/refined/src/main/scala-3/.../RefinedMacro.scala`: `minimum`/`maximum`/`multipleOf` take the
     original value and the base type, `num(v: Double)` (line 188) becomes a by-type `Expr` (`Expr(v.longValue)`,
     `Expr(v.floatValue)`, `Expr(v.doubleValue)`); `asNumber` stays for comparisons and the error messages.
   - Output: `Int Refined Greater[17]` → `"exclusiveMinimum": 17` (README:759 already claims this; today `17.0`),
     `Double Refined Greater[1.5]` → `1.5`, `Double Refined Positive` → `0.0`, `Long Refined Less[9007199254740993L]` →
     exact. Refined tree specs (`RefinedNumbersSpec`, ...) are unaffected: `Def` equality compares the rendered `Value`
     (`num` equality is numeric).
   - Alternative (smaller): both macros keep passing a `Double` (Scala 3 emits a `Double` literal instead of
     `Double.valueOf`), output stays `16.0` everywhere, refined's `Long` precision loss stays. Lost: integer bounds on an
     `integer` schema keep the `.0` and README:759 stays wrong.
8. **Tests** (all shared `src/test/scala` unless noted, so they run on JVM and JS, all three Scala versions):
   - `core/src/test/scala/com/github/andyglow/json/DecimalsSpec.scala`: update the pinned `Value.num(1.1f)` →
     `"1.1"` and `Value.num(Integer.valueOf(16): Number)` → `"16"`; add `fromFloat` cases (`0.1f` → `0.1`, `16f` →
     `16.0`, `1e10f` → `1.0E+10`, `1.0E-4f`, `Float.MaxValue` → `3.4028235E+38`, `Float.MinPositiveValue` →
     `1.4E-45`, `-0.0f` → `0.0`, NaN throws) and `num(x: Number)` cases (boxed `Long.MaxValue`, `Long.MinValue`, `20`,
     `16.0` → `16`, `0.25`, `1e300` → `1.0E+300`, `0.1f` → `0.1`, `scala.BigDecimal`, `BigInt`, `java.math.BigDecimal`,
     `java.math.BigInteger`, NaN throws).
   - new `api/src/test/scala/com/github/andyglow/jsonschema/NumberPrecisionSpec.scala`: the repro table above as
     rendered Draft 07 text (each of the five bounds keywords at least once; every overload: `:= 20`, `5L`,
     `Long.MaxValue`, `16.0` → `16.0`, `1.5`, `0.1f` → `0.1`, a `Short`, `BigInt`, `BigDecimal`, a `java.lang.Long`
     value and a `Number` value (boxed `16.0` → `16`, the documented `Number` rule); `Float` default; `@typeHint[Long]` family
     with `ToValue mk { case A => 1L; case B => Long.MaxValue }` → `"enum": [1, 9223372036854775807]`; the implicit
     conversions `1L`, `Long.MaxValue`, `BigDecimal`, `0.1f`, `5.toShort`, `16.0`).
   - new `api/src/test/scala-jvm/com/github/andyglow/jsonschema/NumberPrecisionValidationSpec.scala` (networknt, JVM
     only): that family's schema accepts `9223372036854775807` and a `ToValue` enum of `0.1f` accepts `0.1`, on Draft
     04/07/2020-12 (both fail today).
   - `modules/circe-json/src/test/.../AsCirceSpec.scala`: for `Long.MaxValue`, `5L`, a 30-digit `BigInt`, a 20-digit
     `BigDecimal`, `0.1f`, `0.1`, `16.0`: `JsonFormatter.format(AsCirce.toValue[X].apply(x)) shouldBe x.asJson.noSpaces`
     (platform-independent: it states "what circe writes", which differs on JS for `16.0`/`0.1f`); plus one rendered
     schema with those defaults.
   - `modules/u-json/src/test/.../AsUSpec.scala`: the same check against `upickle.default.write(x)` (big numbers are
     strings there, and match).
   - play/spray: add the same "default equals what the library writes" check as a regression guard (play's
     `BigInt` uses exponent form: compare as numbers there, or leave `BigInt` out). json4s: nothing to change.
   - refined: a spec with the step-7 outputs as rendered text (`Int Refined Greater[17]`, `Double Refined Greater[1.5]`,
     `Long Refined Less[9007199254740993L]`, `Int Refined Divisible[20]`), on each Scala version (2.12 with `W.`).
   - E4 overlap (#473, `gap/e4-boxed-bigmath`, not merged): E4 adds an AsCirceSpec comment saying the bridge "reads
     every circe number with `toDouble`" and `JFloatV = num(x.floatValue)` (fixed by P2 as is). Whichever PR merges
     second updates that comment/test and adds a boxed `java.lang.Long.MAX_VALUE` default to the circe check above.
9. **Golden files**: `modules/refined/src/test/resources/refined/fs-example.draft07.json`: `"minimum": 16.0` →
   `16`, `"maximum": 512.0` → `512` (an `Int` refined base, step 7). Run
   `GOLDEN_UPDATE=1 sbt clean api/test` on each Scala version to catch any other golden; expected: none (no golden or
   golden-legacy file has a whole-number bound or a `Float`/`Long` value with a fraction; checked with grep).
   Tree specs that compare `Value` trees aren't affected (`num` equality is `BigDecimal` equality: `16 == 16.0`).
10. **Docs** (README and paradox say the same things):
   - README "Scala.js" list (line 77) and `docs/src/main/paradox/getting-started.md:38`: keep "a `Double` bound of
     `16.0` is `16.0`" and add: a bridge's `ToValue` gives what the library writes on that platform (circe, play-json and
     upickle write `16.0` as `16` and `0.1f` as `0.10000000149011612` on Scala.js).
   - README "Validation" and `docs/src/main/paradox/validation.md`: bounds keep their exact value and print as their
     type does (`maximum := 20` gives `20`, `16.0` gives `16.0`, `Long.MaxValue` and `BigDecimal`s exact); a bound passed
     as a boxed `java.lang.Number` renders a whole value as an integer.
   - README refined example (line 759) is now true as written; check the paradox refined page says the same.
   - README bridges paragraph ("The bridges also provide `ToValue`...", line 573) and
     `docs/src/main/paradox/rendering.md`: numbers keep the library's value and text; `asU` goes through
     `ujson.Num`, a `Double`, so numbers beyond 2^53 or with more than 17 significant digits lose precision in
     `asU`'s output (use `stringify` or another bridge).
   - `docs/src/main/paradox/migration-0.9.md`-style note is the lead's call (no CHANGELOG edit here); the PR body
     lists the output changes.
11. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9 (`clean`: refined's macros change);
   `rootJS/test` on all three (core is touched); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll` (the JS `Decimals` is
   in `scala-js`); `makeDocs`.

Scala 2 vs 3: the core changes are runtime code shared by both; refined's two macros change in step with each other
(step 7), and the refined golden is shared by both, which pins that they agree. Scala.js: the `Decimals.fromFloat`
shim (step 2) and the class-reliability rule of the `Number` overload (P1-A); every new spec runs on JS (except the
networknt one).

Neighbours (not in scope, for the lead):
- E4 (#473, boxed `java.lang.Float`/`Long`/`java.math.*` `ToValue`s) and E6 (`Float` literal types) get the exact
  values from P1/P2 without further change (see step 8 for the E4 test overlap).
- D2 (#491, `Long` → `integer`): with P3-A its `@typeHint[Long]` enum renders `[1, 2]`, not `[1.0, 2.0]` (D2-R6).

Risks: snapshot tests on users' side that compare schema text see `20.0` → `20` and the other changes below;
`Value.num(x: Number)` is public and its output changes for whole values; JDK 8–18 may print longer `Float` digits
(still the same `Float`); the Scala.js float-valued-double corner of the `Number` overload; a user's own call
`maximum := (x: Number)` keeps going through `Number` (whole → integer).

## Compatibility

- Source: additions only. The numeric keywords gain typed `:=` overloads; `:= 'c'` now compiles (`Char` widens to
  `Int`, renders `99`); no call that compiled before resolves differently except through the new, more specific
  overloads (probe: 2.12/2.13/3). An unapplied `maximum.:= _` with no expected type becomes ambiguous (with an expected
  type, or as `xs.map(maximum := _)`, it still resolves; r-b19's N1): rare, list it in the implementation PR.
- Binary: the five numeric case objects get a new superclass `NumericInstance` (between them and `Instance`) and six
  methods; nothing is removed. Binary-compatible (no MiMa in the build). refined's macros emit different code at user call
  sites; they need the new core (same release).
- Output (all profiles, Legacy included; JVM and Scala.js unless said):
  - bounds: `Int`/`Short`/`Byte` bounds lose the `.0` (`"maximum": 20.0` → `20`); `Double` bounds unchanged (`16.0`,
    `1.5`); `Long`/`BigInt`/`BigDecimal` bounds exact (`5.0` → `5`, `9.223372036854776E+18` → `9223372036854775807`);
    `Float` bounds shortest (`0.10000000149011612` → `0.1`); a boxed `Number` bound: whole → integer (`16.0` → `16`);
  - refined bounds on integral bases: `17.0` → `17`; `Long` bounds exact;
  - `Float` defaults and values: shortest (`0.10000000149011612` → `0.1`, `1.100000023841858` → `1.1`);
  - values from the implicit conversion (`ToValue mk { case A => 1L }`, `obj("a" -> 5L)`): `1.0` → `1`, big
    `Long`/`BigDecimal` exact;
  - circe `ToValue`: `5.0` → `5`, big `Long`/`BigInt`/`BigDecimal` exact; on Scala.js whole `Double`s `16.0` → `16`
    (circe's JS output);
  - upickle `ToValue`: whole numbers `5.0` → `5`, `16.0` → `16`;
  - golden changes: `refined/fs-example.draft07.json` only; no `golden-legacy` change.
- PVP: API additions only (minor-compatible; no removal or change), so a patch/minor by the API rule; the JSON values
  are equal or corrected. The text output does change
  for the default profile; if the release treats that as breaking (as B3 #470 and D2 #491 do), it rides the same minor
  bump. The lead decides.

## Review log
Round 1 (r-b19, on bd7b4f6):
- B19-R1 (must) typed `:=` overloads for P1 → accepted: new option P1-D, WON (r-b19's probe on 2.12/2.13/3); P1-A kept
  only for the inherited `Number` overload; P1-C's "can still pick it" removed (D covers it). (a) refined: both macros
  emit bounds at the refined base type (new step 7, with the smaller "keep `Double`" alternative); golden
  `fs-example` goes `16.0` → `16` from an `Int` base, the same on Scala 2 and 3. (b) `:= 'c'` compiles: in
  Compatibility. (c) new superclass and methods: in Compatibility. README:77 / getting-started stay as written.
- B19-R2 (nit) `BigDecimalAsIfIntegral` falls through → accepted: match `Numeric.BigDecimalIsConflicted`, plus a
  runtime-class fallback (`BigDecimal`, `BigInt`, `java.lang.Long`) before `n.toDouble`.
- B19-R3 (nit) `fround` wording → accepted: step 2 uses `.toFloat`, as P2-A and the probe.
- B19-R4 (nit) `AtomicLong`/`LongAdder` → accepted: stated out of scope (they keep `doubleValue`, as today).
- B19-R5 (nit) E4 overlap → accepted: step 8 says the second PR to merge updates E4's AsCirceSpec comment/test and adds a
  boxed `java.lang.Long.MAX_VALUE` default.

Round 2 (r-b19, on 504a04b): approved. Non-blocking: N1 (eta-expansion ambiguity) added to Compatibility; N2 (API
additions are a minor bump under PVP, 0.9.x → 0.9.(x+1)) matches "patch/minor" in Compatibility.
