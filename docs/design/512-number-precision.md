# Numbers keep their exact value on the way to JSON  (issue #512, item B19)
Status: draft

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

#### A. Exact value, whole numbers as integers — WON
`num(x: Number)` dispatches on the class where the class is reliable on both platforms, and on the value otherwise:

```scala
def apply(x: Number): num = new num(x match {
  case x: scala.math.BigDecimal => x
  case x: scala.math.BigInt     => BigDecimal(x)
  case x: java.math.BigDecimal  => BigDecimal(x)
  case x: java.math.BigInteger  => BigDecimal(BigInt(x))
  case x: java.lang.Long        => BigDecimal(x.longValue)
  case x                        =>                     // Integer, Short, Byte, Float, Double, other Numbers
    val d = x.doubleValue
    if (d.isWhole && d > -9.2e18 && d < 9.2e18) BigDecimal(d.toLong)  // 20 -> 20, 16.0 -> 16, 1e18 -> 1000000000000000000
    else if (x.isInstanceOf[java.lang.Float]) Decimals.fromFloat(x.floatValue)
    else Decimals.fromDouble(d)                                     // 1.5, 1.0E+300; NaN/Infinity still throw
})
```

Output: `maximum := 20` → `20`, `5L` → `5`, `Long.MaxValue` → `9223372036854775807`, `BigDecimal`/`BigInt` exact,
`0.1f` → `0.1`, `1.5` → `1.5`, `16.0` → `16`. Identical on the JVM and on Scala.js, and for every profile.
Pros: small (one method, plus `Decimals.fromFloat` from P2), no API change, exact values, integer bounds look like integers.
Cons: a `Double` bound with a whole value (`maximum := 16.0`) renders `16`, not `16.0`; README/getting-started say
"a `Double` bound of `16.0` is `16.0` in the JSON" and need rewording (the parity they promise still holds). Same JSON
value, so no validator sees a difference. Scala.js corner: a `Double` bound whose value is exactly representable as a
`Float` *and* needs more than 9 digits (e.g. `0.10000000149011612`) renders as that `Float`'s digits (`0.1`) there,
because the boxed value can't tell the two apart; nobody writes such a literal on purpose.

#### B. Type-based dispatch on the boxed class — LOST
`Integer`/`Short`/`Byte` → integer, `Double` → `Decimals.fromDouble`, `Float` → `fromFloat`, the rest as in A.
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
probe, and buys only the `16.0`-vs-`16` layout of whole `Double` bounds over A. The owner can still pick it if
`16.0` must stay `16.0`; A doesn't block a later move to C.

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
  case _: Numeric.BigDecimalIsFractional => num(x.asInstanceOf[BigDecimal])
  case _                                 => num(n.toDouble(x))       // other Numerics (Char, user-defined): as today
}
```

The traits exist under these names in 2.12 and 2.13 (Scala 3 uses the 2.13 library); the probe
(`B19PlatformProbeSpec`) matches all eight, and `Char` falls through, on 2.12 and 3.3.9, JVM and JS. Output: `1L` → `1`,
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
too. The layout part (`maximum := 20` → `20` instead of `20.0`) is the same JSON number. No `golden-legacy` file
contains such a number (checked: `grep` finds none), so no Legacy golden changes. Alternative: Legacy keeps `20.0` for
whole non-`Long` bounds, which needs the renderer to re-render `Def.value` under the profile (`AsDraftSupport`); not
worth it for a layout no validator distinguishes, but listed for the lead.

## Decision

One rule: a number keeps its exact value and renders the way its Scala type prints it, the same on the JVM and on
Scala.js. Bounds (P1-A), whose boxed type is unreliable on Scala.js, keep the exact value and render whole numbers as
integers. `Float`s render as their shortest round-tripping decimal (P2-A), the implicit conversion follows the
`Numeric`'s type (P3-A), and the circe and upickle bridges show the number exactly as the library writes it (P4-A,
P5-A). play, spray and json4s already do.

## Implementation plan

1. **`core/src/main/scala-jvm/com/github/andyglow/json/Decimals.scala`**: add
   `def fromFloat(f: Float): BigDecimal = BigDecimal(java.lang.Float.toString(f))` (NaN/infinity throw, as `fromDouble`).
2. **`core/src/main/scala-js/com/github/andyglow/json/Decimals.scala`**: factor the layout part of `fromDouble` into
   `private def layout(digits: String, exp: Int, negative: Boolean): BigDecimal`; add `fromFloat(f)`: NaN/infinity →
   `BigDecimal(f.toDouble)` (throws), `0` → `BigDecimal(BigInt(0), 1)`, else the smallest `p` in 1..9 with
   `js.Math.fround(d.toPrecision(p).toDouble) == d`, take its digits/exponent (`toExponential(p - 1)`), `layout(...)`.
   Keep the scaladoc style of the file (why JS differs).
3. **`core/src/main/scala/com/github/andyglow/json/Value.scala`**:
   - `num.apply(x: Float) = new num(Decimals.fromFloat(x))`;
   - `num.apply(x: Number)`: the dispatch in P1-A, with a scaladoc saying why whole values become integers (the boxed
     class is unreliable on Scala.js);
   - `ValueAdapter.numberAdapter`: the `Numeric` match in P3-A.
4. **`modules/circe-json/src/main/scala/com/github/andyglow/jsonschema/AsCirce.scala`**: P4-A in `toValue`
   (`Decimals` is `private[andyglow]`, reachable from `com.github.andyglow.jsonschema`).
5. **`modules/u-json/src/main/scala/com/github/andyglow/jsonschema/AsU.scala`**: P5-A in `numAdapter`'s `unadapt`.
6. **Tests** (all shared `src/test/scala` unless noted, so they run on JVM and JS, all three Scala versions):
   - `core/src/test/scala/com/github/andyglow/json/DecimalsSpec.scala`: update the pinned `Value.num(1.1f)` →
     `"1.1"` and `Value.num(Integer.valueOf(16): Number)` → `"16"`; add `fromFloat` cases (`0.1f` → `0.1`, `16f` →
     `16.0`, `1e10f` → `1.0E+10`, `1.0E-4f`, `Float.MaxValue` → `3.4028235E+38`, `Float.MinPositiveValue` →
     `1.4E-45`, `-0.0f` → `0.0`, NaN throws) and `num(x: Number)` cases (boxed `Long.MaxValue`, `Long.MinValue`, `20`,
     `16.0` → `16`, `0.25`, `1e300` → `1.0E+300`, `0.1f` → `0.1`, `scala.BigDecimal`, `BigInt`, `java.math.BigDecimal`,
     `java.math.BigInteger`, NaN throws).
   - new `api/src/test/scala/com/github/andyglow/jsonschema/NumberPrecisionSpec.scala`: the repro table above as
     rendered Draft 07 text (each of the five bounds keywords at least once; `Float` default; `@typeHint[Long]` family
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
7. **Golden files**: `modules/refined/src/test/resources/refined/fs-example.draft07.json`: `"minimum": 16.0` →
   `16`, `"maximum": 512.0` → `512` (refined passes bounds as `Double`s; whole → integer). Run
   `GOLDEN_UPDATE=1 sbt clean api/test` on each Scala version to catch any other golden; expected: none (no golden or
   golden-legacy file has a whole-number bound or a `Float`/`Long` value with a fraction; checked with grep).
   Tree specs that compare `Value` trees aren't affected (`num` equality is `BigDecimal` equality: `16 == 16.0`).
8. **Docs** (README and paradox say the same things):
   - README "Scala.js" list (line 77) and `docs/src/main/paradox/getting-started.md:38`: replace "a `Double` bound of
     `16.0` is `16.0` in the JSON, not `16`" with: numbers render the same as on the JVM (a `Double` default `16.0` is
     `16.0`); a bridge's `ToValue` gives what the library writes on that platform (circe, play-json and upickle write
     `16.0` as `16` and `0.1f` as `0.10000000149011612` on Scala.js).
   - README "Validation" and `docs/src/main/paradox/validation.md`: bounds keep their exact value (`Long`, `BigInt`,
     `BigDecimal`), and whole numbers render as integers (`maximum := 20` gives `20`, `maximum := 16.0` gives `16`).
   - README bridges paragraph ("The bridges also provide `ToValue`...", line 573) and
     `docs/src/main/paradox/rendering.md`: numbers keep the library's value and text; `asU` goes through
     `ujson.Num`, a `Double`, so numbers beyond 2^53 or with more than 17 significant digits lose precision in
     `asU`'s output (use `stringify` or another bridge).
   - `docs/src/main/paradox/migration-0.9.md`-style note is the lead's call (no CHANGELOG edit here); the PR body
     lists the output changes.
9. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core is
   touched); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll` (the JS `Decimals` is in `scala-js`); `makeDocs`.

Scala 2 vs 3: no macro change; everything is runtime code shared by both. Scala.js: the `Decimals.fromFloat` shim (step
2) and the class-reliability rule in P1-A; every new spec runs on JS (except the networknt one).

Neighbours (not in scope, for the lead):
- refined converts its literal bounds to `Double` in both macros (Scala 2 `AST.scala:96/103` `vv: Double`, Scala 3
  `RefinedMacro.scala:188` `java.lang.Double.valueOf`), so `Long Refined Less[9007199254740993L]` still loses
  precision before `num` sees it. Passing the literal as its own boxed type would fix it on top of P1-A.
- E4 (#473, boxed `java.lang.Float`/`Long`/`java.math.*` `ToValue`s) and E6 (`Float` literal types) get the exact
  values from P1/P2 without further change.
- D2 (#491, `Long` → `integer`): with P3-A its `@typeHint[Long]` enum renders `[1, 2]`, not `[1.0, 2.0]` (D2-R6).

Risks: snapshot tests on users' side that compare schema text see `20.0` → `20` and the other changes below;
`Value.num(x: Number)` is public and its output changes for whole values; JDK 8–18 may print longer `Float` digits
(still the same `Float`); the Scala.js float-valued-double corner of P1-A.

## Compatibility

- Source and binary: no signature changes (method bodies only). No MiMa in the build.
- Output (all profiles, Legacy included; JVM and Scala.js unless said):
  - bounds: whole values lose the `.0` (`"maximum": 20.0` → `20`, `16.0` → `16`); `Long`/`BigInt`/`BigDecimal` bounds
    exact (`9.223372036854776E+18` → `9223372036854775807`); `Float` bounds shortest (`0.10000000149011612` → `0.1`);
  - `Float` defaults and values: shortest (`0.10000000149011612` → `0.1`, `1.100000023841858` → `1.1`);
  - values from the implicit conversion (`ToValue mk { case A => 1L }`, `obj("a" -> 5L)`): `1.0` → `1`, big
    `Long`/`BigDecimal` exact;
  - circe `ToValue`: `5.0` → `5`, big `Long`/`BigInt`/`BigDecimal` exact; on Scala.js whole `Double`s `16.0` → `16`
    (circe's JS output);
  - upickle `ToValue`: whole numbers `5.0` → `5`, `16.0` → `16`;
  - golden changes: `refined/fs-example.draft07.json` only; no `golden-legacy` change.
- PVP: no API change, so a patch by the API rule; the JSON values are equal or corrected. The text output does change
  for the default profile; if the release treats that as breaking (as B3 #470 and D2 #491 do), it rides the same minor
  bump. The lead decides.

## Review log
