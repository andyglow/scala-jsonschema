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
| `LongHinted` field | `{"type":"number","enum":[1.0,2.0]}` (`[1, 2]` with a `ToValue` that returns `Value.num(1L)`: the formatting depends on the `ToValue`) |
| `Either[Long, Double]` | `oneOf[{value: number}]`: one member, because `number`'s `equals` ignores `T`, so the `Set` deduped the two branches at derivation |
| `Either[Int, Long]` | `oneOf[{value: integer}, {value: number}]`: rejects `{"value": 1}` (B14) |

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
  (`@typeHint[Long]` → `{"type":"integer","enum":[1,2]}`). There are two ways to build such an `enum`: a plain `enum` node
  (`Lowering.apply`) and the `enum` that `familyAt` builds from a family's singletons. Both go through the same function.
  `familyAt` merges singletons on their value schemas *before* the integral step, then lowers the built `enum` (D2-R4).
  The whole-values guard matters because `ValueSchema.schemaForNum` (`Schema.enum.of(Value.num(1.5))`) and Scala 2's
  `UJsonValueType` (a `ToValue` value typed `Value.num`) type an arbitrary number as `` `number`[Long] ``, and a
  `@typeHint[Long]` with a `ToValue` that returns `1.5` would lie the same way. (A `@typeHint[Value.num]` doesn't get here:
  both macros derive `Value.num` as a case class, `{"type":"object","enum":[1.5,2]}`. That's a neighbour bug, reported.)
- **`number`'s `equals` becomes integrality-aware** (D2-R1): `` `number`[Long] == `number`[Short] `` (both integral) but
  `` != `number`[Double] ``. `hashCode` stays `"number".hashCode`. Without this, the derivation's `Set`s (`Either`, `oneof`)
  and `familyAt`'s merge collapse a `Long` branch and a `Double` branch into one, and lowering then turns the survivor into
  `integer`. `Either[Long, Double]` would reject `{"value": 1.5}`, and `Either[Double, Long]` would not: the result would
  depend on order. With the change, both branches survive derivation. Legacy still renders the 0.7 output: its `AsNumber`
  step turns the `Long` branch into `` `number`[BigDecimal] ``, which equals the `Double` one, and `oneof(xs map apply)`
  dedupes them again (the implementation must verify it with golden-legacy and a test).
- **Overlapping members** (D2-R1). After lowering, an `integer` member can sit next to a `number` member that differs only
  in integrality. `{"x": 1}` matches both, so an exclusive `oneOf` rejects it. This is the problem `Int` already has today
  (`Either[Int, Double]`, B14), now extended to `Long`/`Short`/`Byte`/`BigInt`:
  - sealed families (`FA(x: Long)`, `FB(x: Double)`): B3 (#470) runs its disjointness check on the lowered members and
    treats `integer ⊂ number` as an overlap, so the family renders `anyOf`, which is valid. **D2 lands after B3.**
  - `Either[Long, Double]` and plain `oneof`s: B14's scope (exclusive `oneOf` with overlapping members). Without B14, D2
    makes a schema that accepts every valid document today invalid. **`Either[Long, Double]` would reject `{"value": 1}`**
    (a valid `Left(1L)`), where today's one-member `oneOf[{value: number}]` accepts everything (D2-R10). So **D2 lands after
    B14**, or, if the owner accepts it, with a test that pins `Either[Long, Double]` as a known B14 case (tag `TRIAGE-*`,
    next to `Either[Int, Double]`). This is an open question for the owner in the PR.
  - `Either[Int, Long]` improves: both branches lower to `{value: integer}` and dedupe into one valid member.
- **Draft 04 and play's `BigInt`** (D2-R2). play writes a `BigInt` ≥ 1E20 in exponent form (`1.2345678901234567890123456789E+29`).
  Draft 04 defines `integer` as a number "without a fraction or exponent part", and networknt applies that (it rejects the
  value on Draft 04 and accepts it on 06+). Today's `number` accepts it. Decision: accept and document. It takes Draft 04
  and play and a `BigInt` beyond 1E20, all three. The workaround is `PlayProfile....withIntegral(IntegralStyle.AsNumber)` or
  a local `Predef[BigInt]`. A test pins the behaviour. Alternatives, if the owner wants this closed: a third style
  (`AsIntegerExceptBigInt`) for the play profile, or making `BigInt` alone stay `number` everywhere. Both cost a less uniform
  rule for one corner case.

Sample output (Draft 07):

| schema | Neutral, library profiles | Legacy |
|---|---|---|
| `Long`, `Byte`, `Short`, `BigInt` | `{"type":"integer"}` | `{"type":"number"}` (unchanged) |
| `Option[Long]` field | `{"type":["integer","null"]}` | `{"type":"number"}` (unchanged) |
| `Array[Byte]` | `{"type":"array","items":{"type":"integer"}}` | unchanged |
| `Long` with `minimum := 0` | `{"type":"integer","minimum":0.0}` (validations render through `doubleValue`, B19) | unchanged |
| `@typeHint[Long]` enum | `{"type":"integer","enum":[1.0,2.0]}` (values as the `ToValue` gives them) | unchanged |
| `Either[Long, Double]` | `oneOf[{value: integer}, {value: number}]` (B14 overlap, see above) | unchanged (`oneOf[{value: number}]`) |
| `Either[Int, Long]` | `oneOf[{value: integer}]` (valid; was `[integer, number]`) | unchanged |
| family `FA(x: Long) \| FB(x: Double)` | with B3: `anyOf[{x: integer}, {x: number}]`; today `oneOf[{x: number}]` | unchanged |
| `Double`, `Float`, `BigDecimal`, `Int` | unchanged | unchanged |

Pros: Legacy stays byte-exact, and there is no style change for the lead to approve. The derived trees don't change, so tree
specs and the trees in golden files stay as they are. One rule covers every source of a `number` node (Predefs, E4's Java
types, refined `Short`/`Byte`/`BigInt`, `@typeHint`, `Schema.enum.of(BigInt(1))`, hand-written `` `number`[Long] ``). Users
get a way back (`withIntegral(AsNumber)`). It follows the precedent of E8's `deprecatedKeyword` knob (a Legacy difference
expressed as a profile knob). The sealed trait leaves room for a third style later (see Risks: upickle writes large `Long`s
and every `BigInt` as strings).

Cons: a new public knob (small, additive). The lowered tree holds an `` `integer` `` (a `Schema[Int]`) where the type is
`Long`. The parameter is phantom, but someone matching a `withProfile(Neutral)` tree sees `integer`. The `AsNumber` result
uses `` `number`[BigDecimal] `` as a "resolved" marker: equal to every non-integral `number`, but a trick. `Json.schema[Long].jsonType`
(a tree-level call) still says `"number"`. `number`'s `equals` changes: `` `number`[Long] != `number`[Double] ``, which changes
which members a derived `Set` keeps (rendered output unchanged under Legacy). It depends on B3 (families) and B14 (`Either`)
for the overlapping-member cases.

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

### D. A subclass of the sealed `integer` used by the Predefs  — LOST

In `Schema.scala` (where `integer` is sealed), a `final class` that extends `` `integer` `` and remembers the wider type
(e.g. `` `integer`.Wide[T: Integral] ``). `byteS`/`shortS`/`longS`/`bigIntS` use it (with a `Schema[Long]` cast), and Legacy
lowers it back to `number`.
Pros: the tree says `integer`, so `jsonType` and tree matching agree with the Neutral output (A's `jsonType` con goes
away), and Legacy can still keep `number`.
Cons: every tree spec and golden tree with these types changes. The `Schema[Int]` cast stays. Its `equals` matches Int's
`integer` (`canEqual` is `isInstanceOf[integer]`), so `Either[Int, Long]` would collapse at derivation, and Legacy's
`Either[Int, Long]` would lose a member unless `equals` is overridden too. Refined, ValueSchema and the Scala 2 literal
typing would still need separate alignment. Lost to A for the same output with more change. It's listed so the owner can
pick "the tree says integer" without redoing the design.

### E. Keep `number`, document it  — LOST

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
profile, and keeps it `number` under `AsNumber`, which is Legacy's value. Legacy output stays byte-exact. `number`'s
`equals` learns integrality, so a `Long` and a `Double` member no longer collapse. The overlapping-member cases this
exposes are handled by B3 (families → `anyOf`) and B14 (`Either`), so D2 lands after them.

## Implementation plan

1. `core/src/main/scala/json/Schema.scala`, `` `number` ``: add
   `def isIntegral: Boolean = implicitly[Numeric[T]].isInstanceOf[Integral[_]]` (scaladoc: "the type is integral: `Byte`,
   `Short`, `Long`, `BigInt`, ...; a profile renders it as `integer`, see `JsonSchemaProfile.integralStyle`"). The class is
   `final`, so adding a public method is binary compatible. Make `equals` integrality-aware (D2-R1):
   `override def equals(obj: Any) = obj match { case n: `number`[_] => n.isIntegral == isIntegral && super.equals(obj); case _ => false }`
   (check how `Schema.equals` / `canEqual` combine today and keep validations/texts in the comparison). Keep `hashCode`.
   Then run the whole test suite. A tree spec that compared a `Long` tree with a `` `number`[Double] `` expectation (or the
   reverse) now fails, and must be fixed to the right type parameter, not by loosening `equals`.
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
     `x.remainder(1) == 0`). Leave the `case other` comment accurate (enum handled above). Factor the `enum` case into
     `integralEnum(vs: Schema[_], values: Set[Value]): Schema[_]`: when `vs` is an integral `number` and the guard holds,
     `` `enum`(integral(vs), values) ``; when `vs` is an integral `number` and the guard fails, `` `enum`(vs, values) `` with
     `vs` left as it is. It must **not** be `apply(vs)`, which would turn it into `integer` anyway. A later lowering hits
     the same guard and leaves it, so it renders `number`. Any other `vs`: `` `enum`(apply(vs), values) ``.
   - `familyAt` (D2-R4, D2-R9): singletons merge on their value schema, and the merge key depends on the style, so that
     Legacy merges exactly as 0.7 did:
     - `AsNumber`: `singletonSchema(m) = apply(m.schema)`, as today. An integral `number` becomes `` `number`[BigDecimal] ``,
       which equals every other non-integral `number`. So a `@typeHint[Long]` singleton (1) and a `@typeHint[Double]`
       singleton (2.5) merge into one `enum(number, [1, 2.5])`, the 0.7 output. Then `integralEnum` has nothing left to do;
     - `AsInteger`: `singletonSchema(m)` returns `m.schema` unchanged when it is a `number` (a leaf, nothing else in it to
       lower), else `apply(m.schema)`. Merging uses the integrality-aware `equals`, so the example gives two entries.
       `Merged(vs, values)` → `integralEnum(vs, values.toSet)` gives `{"type":"integer","enum":[1]}` and
       `{"type":"number","enum":[2.5]}`. Both are valid, and their values are disjoint.
     In both cases `` `enum`[Any](vs, values.toSet) `` is replaced by `integralEnum(vs, values.toSet)`, so the guard sees the
     values on this path too. A singleton's `const` path is unchanged (a `const` has no value schema).
     Probe (2.13.18 and 3.3.9, both macros accept per-member hints in a mixed family): `sealed trait M`,
     `@typeHint[Long] case object A` (ToValue `1`), `@typeHint[Double] case object B` (`2.5`), `case class C(x: String)`.
     Today it renders `oneOf[{"type":"number","enum":[1,2.5]}, {object C}]` under Neutral and Legacy.
   - `oneof`: `oneof(xs map apply)` already re-dedupes lowered members. Under `AsNumber`, that's what keeps Legacy's
     `Either[Long, Double]` at one member, the 0.7 output (verify with a test, step 4).
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
     `Schema.enum.of(Value.num(1.5), Value.num(2))` stays `number` (the guard, `Lowering.apply` path); a `@typeHint[Long]`
     family whose `ToValue` gives `1.5` stays `number` (the guard, `familyAt` path, D2-R4), on Scala 2 and 3;
   - the mixed family `M` above (D2-R9), on Scala 2 and 3: Legacy renders the one merged `{"type":"number","enum":[1,2.5]}`
     (byte-exact with today); Neutral renders `{"type":"integer","enum":[1]}` and `{"type":"number","enum":[2.5]}` next to `C`;
   - equality and members (D2-R1): `` `number`[Long] == `number`[Short] ``, `` != `number`[Double] ``;
     `Json.schema[Either[Long, Double]]` keeps two members in the tree; Legacy renders it with one member (the 0.7 output);
     Neutral `Either[Int, Long]` renders one `{value: integer}` member; family `FA(x: Long) | FB(x: Double)` renders `anyOf`
     under Neutral (needs B3);
   - knob: `Neutral.withIntegral(AsNumber)` → `number`, `Legacy.withIntegral(AsInteger)` → `integer` (other Legacy shapes
     kept); `withProfile(Legacy)` then a Neutral render stays `number` (the re-lowering case); `Neutral.lower` is idempotent
     on an integral `number` (`lower(lower(s)) == lower(s)`);
   - refined (`modules/refined` test, 2.13+ dir next to `RefinedQuirksSpec`): `Short Refined Positive` renders `integer`
     under Neutral, `number` under Legacy. The tree assertions in `RefinedQuirksSpec` stay as they are (trees don't
     change); consider renaming that test to "... are `number` nodes".
   - if E4 (#478) is merged: `java.lang.Long`, `java.lang.Short`, `java.lang.Byte`, `java.math.BigInteger` → `integer`,
     `java.math.BigDecimal` → `number`.
5. JVM-only validator checks (`api/src/test/scala-jvm/...`, uses `petshop.JsonValidator`), under Neutral:
   - `Long`: rejects `1.5`, accepts `5` and `9007199254740993` (Draft 04, 07, 2020-12);
   - `Either[Long, Double]`: `{"value": 1}` and `{"value": 1.5}`. Both pass once B14 has landed. Before that, `{"value": 1}`
     is pinned as a known B14 case (a `TRIAGE-*` tag next to `Either[Int, Double]`);
   - family `FA(x: Long) | FB(x: Double)`: accepts `{"x": 1}` and `{"x": 1.5}` (B3's `anyOf`).
6. Existing tests whose expected JSON changes (rendered output, not trees): `AsDraft04Spec` only (D2-R5; the other
   `AsDraftXXSpec`s have no such cases): the `Array[Byte]` items case and the validations `numCase` for `byte`, `short`,
   `long`, `bigInt` → `"integer"` (5 tests in r-d2's prototype on 2.13). Also look for any spec that renders a `Long`
   (search the tests for `Long`, `Short`, `Byte`, `BigInt`; tree comparisons stay as they are, except the equality fallout
   of step 1). `ProfileSpec`: add the knob to any "every knob kept by the builders" test.
7. Goldens: `GOLDEN_UPDATE=1 sbt clean api/test` on 2.12, 2.13, 3.3. Expected diff, in the rendered `draft-*` sections only
   (the `schema` tree sections stay `"kind": "number"`): `golden/shared/DoubleGenericCCWithDefaults[Long].json`,
   `golden/shared/DoubleGenericCCWithPredefinedDefaults[Long].json`, `golden/shared/CRLFSourceSpec.CC.json`,
   `golden/shared/SchemaMacroSpec.Bar9.json` (the `Vector[Long]` / `Long` positions). `golden-legacy/` doesn't change, and
   the Legacy golden run must still pass. Any other diff is a bug. (If E4 is merged, its `golden/shared/boxed/*` change
   too.) Option: add a golden model `shared/IntegerTypes` with all numeric types.
8. Bridges: their profiles all start from `Neutral`, so they get `AsInteger` with no code change. Acceptance (D2-R3, part of
   the plan, not optional):
   - add a `Long` field to the shared petshop model (`api/src/test/scala/petshop/Model.scala`, e.g. `Pet.id: Long`), with
     **small values only** (well inside ±2^53) in every sample. upickle writes a `Long` beyond ±2^53 as a JSON string
     (`"9223372036854775807"`), and both `number` and `integer` reject that, so a large value would fail upickle's acceptance
     for a reason D2 doesn't touch. Regenerate the petshop schemas/outputs of every bridge and list them in the PR;
   - no `BigInt` in the shared model: upickle writes every `BigInt` as a string, so it would fail the same way;
   - `modules/u-json` test: pin the known limitation. `Long.MaxValue` and a `BigInt` are written as strings and rejected by
     the schema (both before and after D2). Name the neighbour item in the test;
   - `modules/play-json` test (D2-R2): `BigInt("123456789012345678901234567890")` as play writes it (`1.2345...E+29`). Draft
     07 and 2020-12 accept it; Draft 04 rejects it, which pins the accepted-and-documented limitation;
   - `modules/circe-json` test: the same `BigInt` (written as plain digits) is accepted on Draft 04, 07 and 2020-12.
9. Order: the implementation lands **after B3 (#470)**, so overlapping families render `anyOf`, and **after B14**, or with
   step 5's pinned B14 case. After rebasing on B3, re-check its disjointness rules treat a lowered `integer` member
   next to a `number` one as an overlap (B3's doc: "integer ⊂ number").
10. Docs (README and paradox say the same):
   - type table (README ~L276, `docs/src/main/paradox/types.md` L10): split into `Byte`, `Short`, `Long`, `BigInt` →
     `integer` (`number` under `JsonSchemaProfile.Legacy`) and `Float`, `Double`, `BigDecimal` → `number`; the same for
     E4's rows if they're merged;
   - profile knob table (README ~L725, `rendering.md` ~L186): a `withIntegral` row, `IntegralStyle.AsInteger`, `AsNumber`,
     "`Byte`/`Short`/`Long`/`BigInt` as `integer`, or as `number` (0.7)";
   - Legacy paragraph (README ~L605, `rendering.md`): Legacy also renders integral types other than `Int` as `number`;
   - a note: Draft 04 + play + a `BigInt` beyond 1E20 (exponent form) is rejected; use `withIntegral(IntegralStyle.AsNumber)`;
   - run `sbt makeDocs`.
11. Gates as in the protocol (core changed: `clean test` on all three + `rootJS/test` on all three, scalafmt).

## Compatibility

- Source: additive (new knob, builder and `IntegralStyle`; new `number.isIntegral`). Nothing existing changes signature.
- Binary: a public method added to the `final class number` (and its `equals` override), and a method with a body added to
  the trait `JsonSchemaProfile`. On 2.12+/3 that's an interface default method, so classes compiled against 0.9.0 that
  implement the trait still link. (The build has no MiMa; this is the same kind of addition as the earlier knobs.)
- Equality: `` `number`[Long] `` no longer equals `` `number`[Double] ``. Code that relied on it (a test comparing trees with
  the wrong type parameter) sees a difference. Derived `Set`s keep a `Long` and a `Double` member apart (`Either[Long, Double]`).
- Output: under `Neutral` (the default) and every library profile, `Byte`/`Short`/`Long`/`BigInt` (and E4's Java
  equivalents) change from `"number"` to `"integer"` in every position, on every draft. A user whose snapshot tests pin
  the old JSON sees a diff. Valid documents stay valid, except fractional values, which were never valid for these types.
  Trees (`Json.schema[...]`) don't change.
- Legacy: byte-exact, no change.
  Listed output changes beyond the type itself: `Either[Int, Long]` goes from two members to one; `Either[Long, Double]` goes
  from one member to two (`integer`, `number`). Until B14 lands, that is **a new invalid case**: it rejects `{"value": 1}`,
  where today it accepts every valid document. Families mixing a `Long` and a `Double` member get `anyOf` (through B3).
  A mixed family's `Long`- and `Double`-hinted singletons render two enums instead of one (Neutral only). Draft 04 + play +
  `BigInt` ≥ 1E20 now fails validation.
- PVP: B3 (#470) treats a change of the default output as breaking (a minor bump, 0.10.0). D2 follows the same rule: 0.10.0,
  in the same release as B3. A 0.9.x patch is possible only if the owner treats it as a pure bug fix (there's no API break).

## Risks and neighbours (not in scope)

- upickle (`UPickleProfile`) writes a `Long` beyond ±2^53 as a JSON string (`ujson.BaseCharRenderer.visitInt64`), and every
  `BigInt`/`BigDecimal` as a string (`Writers.BigIntWriter` writes `toString`). Both `number` (today) and `integer` reject
  those values, so this item doesn't make it worse. A future `IntegralStyle` value (e.g. integer-or-numeric-string) for
  the upickle profile would fix it. Reported to the lead as a separate gap.
- `ToValue mk { case A => 1L }` enum values render as `1.0`/`2.0`. They are equal to `1`/`2` for every validator in drafts
  06+. Under draft-04's lexical "integer" definition, a strict meta-validator could object to `1.0` in an `integer` enum.
  It's a separate formatting issue, noted for the lead.
- Numeric validations render through `Value.num(x: Number) = doubleValue` (B19): `Long` + `minimum := 0` → `0.0`,
  `maximum := Long.MaxValue` → `9.223372036854776E+18` (larger than `Long.MaxValue`). Not D2's bug, but it's why the range
  sub-decision (2) would need B19 first.
- `@typeHint[Value.num]` derives `Value.num` as a case class on both versions (`{"type":"object","enum":[1.5,2]}`,
  probe 2.13.18 + 3.3.9). It's a neighbour bug, reported to the lead.
- E8 (#485) adds a knob to the same `Knobs` lines (merge conflict only). E4 (#478) needs no change: its Java Predefs reuse
  `longS`/`bigIntS`/... nodes. B6's map key patterns are not schema nodes and are unaffected.

## Review log

Round 1 (r-d2, on 1b6a40a):

- D2-R1 (must) `Set` dedupe + lowering makes `Either[Long, Double]` order-dependent and invalid, and a Long/Double family
  invalid → **accepted**. Took r-d2's suggestion (a): `number`'s `equals` becomes integrality-aware, so both branches survive
  derivation, and Legacy re-dedupes after `AsNumber`, so the 0.7 output is kept (plan step 1, test in step 4). The remaining
  `integer`/`number` overlap goes to B3 (families → `anyOf`) and B14 (`Either`, plain `oneof`). New plan step 9 orders D2
  after both, with a pinned B14 case as the fallback. Output changes listed in Option A's table and in Compatibility.
  Validator tests in step 5.
- D2-R2 (must) play's `BigInt` ≥ 1E20 (`1.23E+29`) is rejected by `integer` on Draft 04 → **accepted, decision: accept and
  document**. It needs Draft 04 + play + a `BigInt` beyond 1E20 together; the workaround is `withIntegral(AsNumber)`. Pinned
  by a play test (step 8), documented (step 10). The alternatives (a third style for play, or `BigInt` stays `number`) are
  written in Option A for the owner.
- D2-R3 (must) acceptance coverage → **accepted**. Step 8 is now mandatory: a `Long` field with small values in the shared
  petshop model (and why small: upickle's strings), no `BigInt` there, a u-json test pinning upickle's string limitation,
  play and circe `BigInt` tests.
- D2-R4 (must) the guard is bypassed in `familyAt` → **accepted**. Singletons merge on the value schema before the integral
  step, and the built `enum` goes through the same guarded `integralEnum` (step 2); test on both versions (step 4). Probed
  `@typeHint[Value.num]` on 2.13.18 and 3.3.9: it derives `Value.num` as a case class (`"type":"object"`), so it never
  reaches the guard. Reported as a neighbour bug (Risks).
- D2-R5 (nit) only `AsDraft04Spec` pins these types → **accepted**, step 6 fixed.
- D2-R6 (nit) validations render as `0.0`; enum value formatting depends on the `ToValue` → **accepted**: sample rows
  fixed, B19 noted in Risks (and why ranges would need it first).
- D2-R7 (nit) no MiMa; PVP like B3 → **accepted**: MiMa sentence rephrased; PVP: 0.10.0 with B3, a patch only if the owner
  calls it a bug fix.
- D2-R8 (nit) missed option, a subclass of the sealed `integer` → **accepted**, added as Option D (LOST, with why). The old
  "keep `number`" option is now E.

Round 2 (r-d2, on c15a7ef):

- D2-R9 (must) the integrality-aware `equals` splits `familyAt`'s singleton merge, so Legacy isn't byte-exact →
  **accepted**. The merge key now depends on the style (step 2). Under `AsNumber` it is the lowered value schema
  (`number[BigDecimal]`, so a Long-hinted and a Double-hinted singleton merge as in 0.7); under `AsInteger` it is the raw
  one (two enums, `integer` [1] and `number` [2.5]). Probed on 2.13.18 and 3.3.9: both macros accept per-member hints in a
  mixed family, and today's output is one `number` enum [1, 2.5]. Test added (step 4). While fixing it, I also corrected
  `integralEnum`'s guard-fails branch: it keeps `vs` as it is, not `apply(vs)`, which would have made it `integer` anyway.
- D2-R10 (nit) say plainly that `Either[Long, Double]` becomes invalid without B14 → **accepted**: Option A and
  Compatibility say "a new invalid case"; it goes in the PR's open questions.
