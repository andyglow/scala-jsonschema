# Byte, Short, Long and BigInt as `integer`  (issue #491, item D2)
Status: draft

## Problem

`Int` renders as `"type": "integer"`. The other integral types, `Byte`, `Short`, `Long` and `BigInt`, render as
`"type": "number"`, under every draft and every profile (Neutral and Legacy). It's the same on 2.12, 2.13 and 3.3, JVM and JS.
The cause is in `core/src/main/scala/json/schema/Predef.scala`:

```scala
implicit val byteS: Predef[Byte]     = Predef(`number`[Byte])
implicit val shortS: Predef[Short]   = Predef(`number`[Short])
implicit val intS: Predef[Int]       = Predef(`integer`)
implicit val longS: Predef[Long]     = Predef(`number`[Long])
implicit val bigIntS: Predef[BigInt] = Predef(`number`[BigInt])
```

Repro (Draft 07, `JsonSchemaProfile.Neutral`; Legacy is the same, except for 0.7's `Option` shapes):

```scala
case class Nums(b: Byte, s: Short, i: Int, l: Long, bi: BigInt, d: Double, bd: BigDecimal)
case class Wrap(xs: Array[Byte], ol: Option[Long], ml: Map[String, Long], ls: List[Short])
case class LongVC(v: Long) extends AnyVal
case class HasVC(id: LongVC, dl: Long = 5L)
@typeHint[Long] sealed trait LongHinted   // A -> 1L, B -> 2L via ToValue
```

| schema | rendered today (2.12 = 3.3) |
|---|---|
| `Json.schema[Long]` (also `Byte`, `Short`, `BigInt`) | `{"type": "number"}` |
| `Nums` | `b`, `s`, `l`, `bi`, `d`, `bd`: `{"type": "number"}`; `i`: `{"type": "integer"}` |
| `Wrap` | `xs`: `{"type":"array","items":{"type":"number"}}`, `ol`: `{"type":["number","null"]}`, `ml`: `patternProperties {"^.*$": {"type":"number"}}`, `ls`: `items number` |
| `HasVC` | `id`: `{"type":"number"}`, `dl`: `{"default":5,"type":"number"}` |
| `LongHinted` field | `{"type":"number","enum":[1.0,2.0]}` |

networknt json-schema-validator accepts `1.5` as a valid `Long` (repro test against `Json.schema[Long]`, Draft 07).

The code base is already inconsistent about it:
- refined: `Long Refined P` → `integer` since 0.7 (PR #269, "refined longs should have integer type not number"), but
  `Short` / `Byte` / `BigInt Refined P` → `number` (`RefinedQuirksSpec` pins it);
- `ValueSchema` (behind `Schema.enum.of`): `Int`, `Long`, `Byte`, `Short` → `integer`; `BigInt` → `number`;
- enumeratum: `IntEnum`, `LongEnum`, `ShortEnum`, `ByteEnum` → `integer`;
- Scala 2 macro, enum values typed from literals (`UJsonValueType`): `Int`/`Byte`/`Short` → `integer`, `Long`/`BigInt` → `number`.

## Why it is a problem

- The schema is looser than the type: validators accept `1.5` for a `Long` or `Byte` field. Every user who validates input
  with the generated schema hits it.
- Code generators reading the schema (OpenAPI/JSON Schema → TypeScript, Kotlin, Java, ...) emit a floating type
  (`double`, `number`) for an id field that is a `Long`. Peers get this right: tapir maps `Byte`/`Short`/`Int`/`Long`/
  `BigInt`/`java.math.BigInteger` to `integer` (`notes/reports/gaps-types.md` §4).
- `Long Refined Positive` is `integer` while plain `Long` is `number`. Adding a refinement shouldn't change the JSON type.

Valid documents never fail validation because of this: every integer is also a `number`. So the output is too loose, not
wrong. That is the reason for the Legacy question below.

## Options

### A. The profile decides: an `integralStyle` knob, applied when the schema is lowered  — WON

Derivation and Predefs stay as they are: the tree keeps `` `number`[Long] ``, and the node already knows its `Numeric[T]`.
A new profile knob decides, at lowering time (`JsonSchemaProfile.lower`, the step that already shapes `option` and
`family`), what a `number` of an integral type becomes:

```scala
sealed trait IntegralStyle
object IntegralStyle {
  case object AsInteger extends IntegralStyle  // `"type": "integer"`: Neutral and every library profile
  case object AsNumber  extends IntegralStyle  // `"type": "number"`: Legacy (0.7)
}
def integralStyle: IntegralStyle = IntegralStyle.AsInteger         // on the trait, so hand-written profiles still compile
final def withIntegral(x: IntegralStyle): JsonSchemaProfile = ...  // builder, like withSingleton
```

- "Integral" means `implicitly[Numeric[T]].isInstanceOf[Integral[_]]`. That holds for `Byte`, `Short`, `Long`, `BigInt`
  (`Numeric.ByteIsIntegral`, ..., `BigIntIsIntegral`), and also for E4's `java.lang.{Byte,Short,Long}` and
  `java.math.BigInteger`, which reuse those nodes. It doesn't hold for `Float`, `Double`, `BigDecimal` (`...IsFractional`).
  It also holds for a user's `` `number`[T] `` whose `Numeric` is an `Integral` (e.g. spire's `SafeLong`). That is intended:
  the type is integral.
- `AsInteger`: the node becomes `` `integer` `` with the same validations, title, description and key (`withExtraFrom`).
- `AsNumber`: the node becomes `` `number`[BigDecimal] `` with the same extras. It renders the same `"type": "number"` but
  isn't integral. That matters because the renderers lower again with Neutral (`AsDraftSupport.lowered`), and that second
  pass must not turn a Legacy-shaped schema back into `integer`. `number`'s `equals`/`hashCode` ignore `T`, so the lowered
  tree is still equal to the derived one.
- An `enum` whose value schema is an integral `number` and whose values are all whole numbers gets the same treatment
  (`@typeHint[Long]` → `{"type":"integer","enum":[1,2]}`). The whole-values guard matters because `ValueSchema.schemaForNum`
  (`Schema.enum.of(Value.num(1.5))`) and Scala 2's `Value.num` type hint type an arbitrary number as `` `number`[Long] ``.

Sample output (Draft 07):

| schema | Neutral, library profiles | Legacy |
|---|---|---|
| `Long`, `Byte`, `Short`, `BigInt` | `{"type":"integer"}` | `{"type":"number"}` (unchanged) |
| `Option[Long]` field | `{"type":["integer","null"]}` | `{"type":"number"}` (unchanged) |
| `Array[Byte]` | `{"type":"array","items":{"type":"integer"}}` | unchanged |
| `Long` with `minimum := 0` | `{"type":"integer","minimum":0}` | unchanged |
| `@typeHint[Long]` enum | `{"type":"integer","enum":[1.0,2.0]}` | unchanged |
| `Double`, `Float`, `BigDecimal`, `Int` | unchanged | unchanged |

Pros: Legacy stays byte-exact, and there is no style change for the lead to approve. The derived trees don't change, so tree
specs and the trees in golden files stay as they are. One rule covers every source of a `number` node (Predefs, E4's Java
types, refined `Short`/`Byte`/`BigInt`, `@typeHint`, `Schema.enum.of(BigInt(1))`, hand-written `` `number`[Long] ``). Users
get a way back (`withIntegral(AsNumber)`). It follows the precedent of E8's `deprecatedKeyword` knob (a Legacy difference
expressed as a profile knob). The sealed trait leaves room for a third style later (see Risks: upickle writes large `Long`s
and every `BigInt` as strings).

Cons: a new public knob (small, additive). The lowered tree holds an `` `integer` `` (a `Schema[Int]`) where the type is
`Long`. The parameter is phantom, but someone matching a `withProfile(Neutral)` tree sees `integer`. The `AsNumber` result
uses `` `number`[BigDecimal] `` as a "resolved" marker: equal to the original, but a trick. `Json.schema[Long].jsonType`
(a tree-level call) still says `"number"`.

### B. Change the Predefs: `byteS`/`shortS`/`longS`/`bigIntS` → `` `integer` ``  — LOST

`Predef(`integer`.asInstanceOf[Schema[Long]])` and so on (the same cast refined's Scala 3 macro and E4 already use). Also align
`ValueSchema.schemaForNumeric`'s `BigInt`, the Scala 2 `UJsonValueType` `Long`/`BigInt` literals, refined's `isInt`
(add `Short`, `Byte`, `BigInt`) and Scala 3's equivalents.

Pros: no new API. The tree itself says `integer`, so `jsonType` and tree matching agree with the output.
Cons: Legacy changes too, and nothing can bring `number` back: once the node is `integer`, `Int` and `Long` look the same,
so no profile can tell them apart. Under the Legacy policy, that's a style change that needs the lead, because the 0.7
output was loose, not invalid. Every tree spec and golden tree that has `` `number`[Long] `` changes (CollectionsSpec,
RefinedQuirksSpec, AsDraftXXSpec, the `schema` sections of the goldens, plus golden-legacy). Changes are spread over
Predef, ValueSchema, both macros and the refined module, both versions.
Lost because A gives the same Neutral output, keeps Legacy, and changes one place.

### C. A generic `integer[T: Integral]` node (`object integer extends integer[Int]`)  — LOST

The model would say what the type is: `` `integer`[Long] ``, `Predef(`integer`[Long])`.
Cons: it breaks binary and source compatibility. `integer` is a `sealed class integer extends Schema[Int]` that users write
as `` `integer` `` and match as `case _: `integer``. A generic class needs `` `integer`[_] `` in patterns, and its
erasure/constructor changes. It also doesn't answer the Legacy question: Legacy would still need a knob to render
`integer[Long]` as `number`. That would be a 0.10.0 change for the same output as A.

### D. Keep `number`, document it  — LOST

Zero risk, but the schema stays looser than the type, and plain and refined `Long` stay inconsistent. The owner already
treated `number` for refined `Long` as a bug in 0.7 (#269).

### Sub-decisions inside A (each can be flipped without redoing the design)

1. **Legacy keeps `number`** (WON) vs **Legacy switches too**: one line (`Legacy`'s `integralStyle = AsInteger`, or drop the
   knob from Legacy). It would regenerate 4 golden-legacy files (listed in the plan). It's a style change under the Legacy
   policy, so the lead has to approve it first.
2. **No range validations** (WON) vs `Byte` → `minimum: -128, maximum: 127`, `Short` → `-32768..32767`: `Int` has no range
   today, tapir adds none, and a `Long` range (±9.2e18) isn't representable in JS doubles. Adding ranges later is a Predef
   change, independent of this knob.
3. **No `format`** (WON): `int32`/`int64` are OpenAPI formats, not JSON Schema ones. Drafts 2019-09/2020-12 would treat
   them as unknown (annotation only).
4. **The knob is a sealed trait** (WON) vs a `Boolean` (`integralAsInteger`, like E8's `deprecatedKeyword`): the trait leaves
   room for a `AsIntegerOrString`-style value, which upickle needs (see Risks).

## Decision

Option A. Derivation stays as it is. A new `JsonSchemaProfile.integralStyle` knob lowers a `number` node of an integral
type (and an integral, whole-valued `enum`) to `integer` under `AsInteger`, the default for Neutral and every library
profile, and keeps it `number` under `AsNumber`, which is Legacy's value. Legacy output stays byte-exact.

## Implementation plan

1. `core/src/main/scala/json/Schema.scala`, `` `number` ``: add
   `def isIntegral: Boolean = implicitly[Numeric[T]].isInstanceOf[Integral[_]]` (scaladoc: "the type is integral: `Byte`,
   `Short`, `Long`, `BigInt`, ...; a profile renders it as `integer`, see `JsonSchemaProfile.integralStyle`"). The class is
   `final`, so adding a public method is binary compatible.
2. `core/src/main/scala/json/schema/JsonSchemaProfile.scala`:
   - `sealed trait IntegralStyle` + `object IntegralStyle { case object AsInteger; case object AsNumber }`, with scaladoc
     (AsInteger: "`Byte`, `Short`, `Long`, `BigInt` as `integer`"; AsNumber: "as `number` (0.7)").
   - Trait: `def integralStyle: IntegralStyle = IntegralStyle.AsInteger`, with scaladoc. Builder
     `final def withIntegral(x: IntegralStyle): JsonSchemaProfile = Knobs(this).copy(integralStyle = x).profile`.
   - `Knobs`: field `integralStyle: IntegralStyle = IntegralStyle.AsInteger`; `override val` in `profile`; copy it in
     `Knobs.apply(p)`. `Legacy = Knobs("legacy", OptionStyle.Legacy, integralStyle = IntegralStyle.AsNumber).profile`.
     (If E8 #485 merged first, it adds `deprecatedKeyword` to the same lines: a textual conflict only.)
   - `Lowering.apply`, before `case other`:
     ```scala
     case n: `number`[_] if n.isIntegral                                      => integral(n)
     case e @ `enum`(n: `number`[_], vs) if n.isIntegral && vs.forall(whole) => `enum`[Any](integral(n), vs).withExtraFrom(e)
     ```
     with `integral(n)` = `AsInteger` → `` `integer`.withExtraFrom(n) ``, `AsNumber` → `` `number`[BigDecimal].withExtraFrom(n) ``
     (comment: the resolved form, so the renderers' second, Neutral lowering leaves it alone), and
     `whole(v) = v match { case Value.num(x) => x.isWhole; case _ => false }`. Check `BigDecimal.isWhole` on 2.12 (else
     `x.remainder(1) == 0`). Leave the `case other` comment accurate (enum handled above).
   - Update the scaladoc that says lowering touches only `option`/`family` (trait header "Everything else ... passes
     through unchanged", `lower`'s "a schema without those nodes comes back equal"): an integral `number` is lowered too.
     The trait header's list of facts gets "this number's type is integral".
3. No change to the Predefs, either macro, refined, enumeratum, ValueSchema or the parser (the parser already maps
   `"integer"` to `` `integer` ``). The Scala 2 / Scala 3 derivation is untouched, so no `clean` subtleties. JS: `isInstanceOf`
   on the `Numeric` instance and `BigDecimal.isWhole` work on Scala.js. The cross-built test in step 4 covers it.
4. New shared spec `api/src/test/scala/com/github/andyglow/jsonschema/IntegerTypesSpec.scala` (all versions, JVM + JS):
   - root `Byte`, `Short`, `Long`, `BigInt` → `integer` under Neutral on Draft 04, 07, 2020-12; `Int` unchanged;
     `Float`, `Double`, `BigDecimal` stay `number`; all of them stay `number` under Legacy;
   - positions: field, `Option[Long]` field (Neutral `["integer","null"]`, Legacy `number`), `Array[Byte]` / `List[Short]`
     items, `Map[String, Long]` value, value class over `Long`, `Long` field with a default;
   - extras kept: `Json.schema[Long].withValidation(`minimum` := 0, `maximum` := 10)`, `.withTitle`/`.withDescription`;
   - enums: `@typeHint[Long]` family → `integer` enum; `Schema.enum.of(BigInt(1), BigInt(2))` → `integer`;
     `Schema.enum.of(Value.num(1.5), Value.num(2))` stays `number` (the whole-values guard);
   - knob: `Neutral.withIntegral(AsNumber)` → `number`, `Legacy.withIntegral(AsInteger)` → `integer` (other Legacy shapes
     kept); `withProfile(Legacy)` then a Neutral render stays `number` (the re-lowering case); `Neutral.lower` is idempotent
     on an integral `number` (`lower(lower(s)) == lower(s)`);
   - refined (`modules/refined` test, 2.13+ dir next to `RefinedQuirksSpec`): `Short Refined Positive` renders `integer`
     under Neutral, `number` under Legacy. The tree assertions in `RefinedQuirksSpec` stay as they are (trees don't
     change); consider renaming that test to "... are `number` nodes".
   - if E4 (#478) is merged: `java.lang.Long`, `java.lang.Short`, `java.lang.Byte`, `java.math.BigInteger` → `integer`,
     `java.math.BigDecimal` → `number`.
5. JVM-only validator check (`api/src/test/scala-jvm/...`, uses `petshop.JsonValidator`): under Neutral, networknt rejects
   `1.5` and accepts `5` and `9007199254740993` for `Long` (Draft 07 and 2020-12).
6. Existing tests whose expected JSON changes (rendered output, not trees): `AsDraft04Spec`, `AsDraft06Spec`,
   `AsDraft07Spec`, `AsDraft09Spec`, `AsDraft12Spec` `numCase` for `byte`, `short`, `long`, `bigInt` → `"integer"`. Also check
   any spec that renders a `Long` (search the tests for `Long`, `Short`, `Byte`, `BigInt`; tree comparisons stay as they are).
   `ProfileSpec`: add the knob to any "every knob kept by the builders" test.
7. Goldens: `GOLDEN_UPDATE=1 sbt clean api/test` on 2.12, 2.13, 3.3. Expected diff, in the rendered `draft-*` sections only
   (the `schema` tree sections stay `"kind": "number"`): `golden/shared/DoubleGenericCCWithDefaults[Long].json`,
   `golden/shared/DoubleGenericCCWithPredefinedDefaults[Long].json`, `golden/shared/CRLFSourceSpec.CC.json`,
   `golden/shared/SchemaMacroSpec.Bar9.json` (the `Vector[Long]` / `Long` positions). `golden-legacy/` doesn't change, and
   the Legacy golden run must still pass. Any other diff is a bug. (If E4 is merged, its `golden/shared/boxed/*` change
   too.) Option: add a golden model `shared/IntegerTypes` with all numeric types.
8. Bridges: their profiles all start from `Neutral`, so they get `AsInteger` with no code change. Recommended: add a `Long`
   field with a small value (e.g. `Pet.id: Long = 1`) to the petshop acceptance model (`api/src/test/scala/petshop/Model.scala`),
   so every library's real codec output is validated against `integer`. That changes the petshop schemas/outputs for all
   bridges: regenerate them and list them in the PR.
9. Docs (README and paradox say the same):
   - type table (README ~L276, `docs/src/main/paradox/types.md` L10): split into `Byte`, `Short`, `Long`, `BigInt` →
     `integer` (`number` under `JsonSchemaProfile.Legacy`) and `Float`, `Double`, `BigDecimal` → `number`; the same for
     E4's rows if they're merged;
   - profile knob table (README ~L725, `rendering.md` ~L186): a `withIntegral` row, `IntegralStyle.AsInteger`, `AsNumber`,
     "`Byte`/`Short`/`Long`/`BigInt` as `integer`, or as `number` (0.7)";
   - Legacy paragraph (README ~L605, `rendering.md`): Legacy also renders integral types other than `Int` as `number`;
   - run `sbt makeDocs`.
10. Gates as in the protocol (core changed: `clean test` on all three + `rootJS/test` on all three, scalafmt).

## Compatibility

- Source: additive (new knob, builder and `IntegralStyle`; new `number.isIntegral`). Nothing existing changes signature.
- Binary: a public method added to the `final class number`, and a method with a body added to the trait
  `JsonSchemaProfile`. On 2.12+/3 that's an interface default method, so classes compiled against 0.9.0 that implement the
  trait still link. MiMa may report the trait addition (`ReversedMissingMethodProblem`), which can be filtered the same way
  as any earlier knob addition.
- Output: under `Neutral` (the default) and every library profile, `Byte`/`Short`/`Long`/`BigInt` (and E4's Java
  equivalents) change from `"number"` to `"integer"` in every position, on every draft. A user whose snapshot tests pin
  the old JSON sees a diff. Valid documents stay valid, except fractional values, which were never valid for these types.
  Trees (`Json.schema[...]`) don't change.
- Legacy: byte-exact, no change.
- PVP: no API break, so a patch release (0.9.x) is allowed. Because the default output changes, I'd ship it with the other
  output changes (B3's 0.10.0), owner's call.

## Risks and neighbours (not in scope)

- upickle (`UPickleProfile`) writes a `Long` beyond ±2^53 as a JSON string (`ujson.BaseCharRenderer.visitInt64`), and every
  `BigInt`/`BigDecimal` as a string (`Writers.BigIntWriter` writes `toString`). Both `number` (today) and `integer` reject
  those values, so this item doesn't make it worse. A future `IntegralStyle` value (e.g. integer-or-numeric-string) for
  the upickle profile would fix it. Reported to the lead as a separate gap.
- `ToValue mk { case A => 1L }` enum values render as `1.0`/`2.0`. They are equal to `1`/`2` for every validator in drafts
  06+. Under draft-04's lexical "integer" definition, a strict meta-validator could object to `1.0` in an `integer` enum.
  It's a separate formatting issue, noted for the lead.
- E8 (#485) adds a knob to the same `Knobs` lines (merge conflict only). E4 (#478) needs no change: its Java Predefs reuse
  `longS`/`bigIntS`/... nodes. B6's map key patterns are not schema nodes and are unaffected.

## Review log
