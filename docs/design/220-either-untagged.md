# `Either` shaped by the profile: an `eitherStyle` knob  (issue #220, item E7)
Status: draft (round 1 answered)

Depends on #470 (B3: `Schema.anyof`, the `OverlappingMembers` knob and its disjointness check). The implementation
goes on top of B3, as E6 does. This doc-only branch stays on `master`.

## Problem

`Either[L, R]` always renders as a `oneOf` of two objects, `{"value": L}` and `{"value": R}`. That is the schema of
the case classes `Left(value: L)` / `Right(value: R)` derived as an untagged sealed family. The JSON libraries don't
write `Either` that way, and no profile can change it: both macros build the `oneof` directly (Scala 2
`USumTypes.eitherOneOf`, Scala 3 `Sums.emitSum` for `SumKind.Either`). It is not a `family` node, so
`JsonSchemaProfile` never sees it (README: "`Either` is not a family (still the 0.7 `oneOf`)").

```scala
case class HasE(e: Either[Int, String])
Json.schema[Either[Int, String]]   // and Json.schema[HasE]
```

Draft-07, identical on 2.12.21, 2.13.18 and 3.3.9, under every profile (Neutral, Legacy, play, circe, upickle,
json4s, spray). Pinned by `golden/groupC/Either[Int,String].json` and `golden/groupC/HasEither.json`, also in
`golden-legacy/`:

```json
{"oneOf": [
  {"additionalProperties": false, "properties": {"value": {"type": "integer"}}, "required": ["value"]},
  {"additionalProperties": false, "properties": {"value": {"type": "string"}},  "required": ["value"]}]}
```

What each library really writes for `Left(1)` / `Right("a")`. Probes in `notes/scratch/c-e7/<module>/EitherProbeSpec.scala`
ran on 2.13.18 and 3.3.9 (spray and circe on 2.12.21 too) and validated each output with networknt against today's
schema under that library's own profile:

| library | `Left(1)` / `Right("a")` | as a field | valid under its profile today (04 / 07 / 2020-12) |
|---|---|---|---|
| upickle 4.4.3 (`upickle.default`, any `AttributeTagged` API) | `[0,1]` / `[1,"a"]` | `{"e":[0,1]}` | 0 of 2 on every draft |
| spray-json (`DefaultJsonProtocol.eitherFormat`) | `1` / `"a"` | `{"e":1}` | 0 of 2 |
| json4s 4.0.7 (`DefaultFormats`; also with `ShortTypeHints` on `Left`/`Right`) | `1` / `"a"` | `{"e":1}` | 0 of 2 |
| circe-generic `auto._` (2.12, 2.13, 3) | `{"Left":{"value":1}}` / `{"Right":{"value":"a"}}` | `{"e":{"Left":{"value":1}}}` | 0 of 2 |
| circe `Encoder.encodeEither("Left", "Right")` (explicit, not implicit) | `{"Left":1}` / `{"Right":"a"}` | | 0 of 2 |
| circe-core alone | no implicit `Encoder[Either]` (doesn't compile) | | |
| play-json 3.0.6 | no `Writes[Either]` (doesn't compile) | | |

How they read it: spray tries both sides and rejects a value both can read ("Ambiguous Either value", e.g. `1` as
`Either[Int, Double]`), which is exactly `oneOf`. json4s takes the first side that reads (`1` as `Either[Int, Double]` is
`Left(1)`), which is `anyOf`. upickle and circe are tagged, so their sides never overlap.

Other shapes probed (2.12 / 2.13 / 3): `Option[Either[..]]`, a recursive `Either[String, Node]`, nested
`Either[Int, Either[String, Boolean]]`, `List[Either[..]]`, a side with an implicit `Schema` (`$ref` inside `value`).
All are the same `{"value": ...}` wrapping, identical across versions, with **one parity difference**:
`Either[Option[Int], String]` has `value: option(integer)` **required** on Scala 2 (Left's field is declared as the
type parameter `A`, so the "an `Option` field isn't required" rule doesn't fire) and **not required** on Scala 3 (the
field type is substituted first). Rendered on draft-07, Scala 3 drops `"required": ["value"]` from the Left member.

A field default of type `Either` needs a `ToValue[Either[..]]`: core has none (compile error asking for one), and with
a bridge import (`AsSpray._`, ...) the default is what the library's codec writes. So defaults already follow the
library and need nothing here.

## Why it is a problem

- #220 (open since 0.7.6, labeled wontfix): the user expected `oneOf[string, Person]` for `Seq[Either[String, Person]]`.
  The owner's answer was that "different json-libs do it different way". That was true and unanswerable in 0.7. Since
  0.8/0.9 the answer to "libraries differ" is a `JsonSchemaProfile` (#88). `Either` is the one shape the profiles still
  skip.
- Under every bridge profile the schema rejects all of that library's own `Either` output (table above). So a
  service that validates its responses against the schema fails on every `Either` field.
- README "Known issues" lists it ("`Either[L, R]` is a `oneOf` of `{"value": L}` and `{"value": R}`"). tapir describes
  `Either` unwrapped (a coproduct of `L` and `R`), so schemas exchanged with tapir users disagree.

## Options

### A. An `either` fact node, lowered by a new profile knob `eitherStyle`  — WON
Do for `Either` what 0.8 did for `Option`: both macros record the fact "this is `Either[L, R]`" as a new
`Schema.either(left, right)` node. `left`/`right` are the schemas of `L` and `R`, resolved under the core rule (an
implicit `Schema` makes a definition, a `Predef` is inlined, anything else is derived). A profile lowers the node when the
schema is rendered (or on `withProfile`), like `option` and `family`. The new knob:

```scala
sealed trait EitherStyle
object EitherStyle {
  case object ValueWrapped extends EitherStyle   // oneOf[{"value": L}, {"value": R}]: 0.7, today. Default (Neutral, Legacy, play)
  case object Untagged     extends EitherStyle   // oneOf[L, R]: spray, json4s, tapir, a hand-written `fold`
  case object AsFamily     extends EitherStyle   // the sealed family Left(value: L) | Right(value: R), shaped by this profile's sum knobs: circe-generic
  final case class WrapperObject(left: String = "Left", right: String = "Right") extends EitherStyle // oneOf[{"Left": L}, {"Right": R}]: circe encodeEither, zio-json
  case object Indexed      extends EitherStyle   // oneOf[[0, L], [1, R]]: upickle
}
trait JsonSchemaProfile { def eitherStyle: EitherStyle = EitherStyle.ValueWrapped; final def withEither(x: EitherStyle): JsonSchemaProfile = ... }
```

Sample output for `Either[Int, String]` in a field, draft-07 (the field position avoids B15's root quirk, where a
root `oneOf` states no `type`):

| style | `"e": ...` |
|---|---|
| `ValueWrapped` | `{"type":"object","oneOf":[{"additionalProperties":false,"properties":{"value":{"type":"integer"}},"required":["value"]}, {... "string" ...}]}` (unchanged) |
| `Untagged` | `{"oneOf":[{"type":"integer"},{"type":"string"}]}`. Sides that can't be proven disjoint get `anyOf` (A5): `Either[String, UUID]` → `{"type":"string","anyOf":[{},{"format":"uuid"}]}` |
| `AsFamily` under circe (`WrapperObject` + `SimpleName`) | `{"type":"object","oneOf":[{"additionalProperties":false,"properties":{"Left":{"type":"object","additionalProperties":false,"properties":{"value":{"type":"integer"}},"required":["value"]}},"required":["Left"]}, {... "Right" ...}]}` |
| `WrapperObject("Left","Right")` | `{"type":"object","oneOf":[{"additionalProperties":false,"properties":{"Left":{"type":"integer"}},"required":["Left"]}, {... "Right": string ...}]}` |
| `Indexed` (draft-04/06/07/2019-09) | `{"type":"array","oneOf":[{"items":[{"type":"integer","enum":[0]},{"type":"integer"}],"additionalItems":false,"minItems":2}, {"items":[{"type":"integer","enum":[1]},{"type":"string"}],"additionalItems":false,"minItems":2}]}` |
| `Indexed` (2020-12) | the same with `"prefixItems": [...]` and `"items": false` in place of `items` / `additionalItems` |

The bridge profiles pick the style their library writes (table in the plan, step 6). Each style is there for a real
encoder: `ValueWrapped` is 0.7, `Untagged` is spray, json4s and tapir, `AsFamily` is circe-generic, `WrapperObject`
is circe's `encodeEither` and zio-json, `Indexed` is upickle.

- Pros: fits the profile design (#88): the macro records facts and the profile decides. One render-time knob covers
  every library and the user's own codec (`SprayProfile.default.withEither(...)`). No change to Neutral or Legacy
  output. The shape lives in one place (the lowering), so Scala 2 and 3 can't diverge again (it fixes the
  `Either[Option[X], _]` parity difference for free).
- Cons: a new case in the sealed `Schema` (core's tree tools, the harness and users' exhaustive matches have to know it,
  as for `option`/`family`/E2's `any`). Every existing `Either` derives to the new node instead of a `oneof`, so user
  code that walks derived trees sees a different node. Breaking under PVP (see Compatibility). `Indexed` needs a
  positional-array node that the model doesn't have (sub-decision A2).

### E. Like A, but the fact as a marker on today's `oneof`  — LOST (the patch-compatible variant)
The same idea as B3's option F. The macros keep emitting today's `oneof[{value: L}, {value: R}]`, mixed with a marker
`sealed trait EitherFact { this: oneof[_] => def left: Schema[_]; def right: Schema[_] }`, built by `oneof.either(l, r)`.
The lowering re-shapes a marked `oneof` by `eitherStyle`. Everything else is as in A (styles, profiles, tests).
- Pros: no new ADT case, and the derived tree has the same node as today. A third-party renderer that ignores the marker
  prints today's output. Source and binary compatible (0.9.x).
- Cons: every rebuild of a `oneof` must keep the marker (`mkCopy`, `duplicate`/`withExtraFrom`, `deepCopy`,
  `Lowering.apply`, B3's ordered rebuilds), the discipline `Free` needs. The marker also holds `left`/`right`
  next to members derived from them, so the two can drift when a user edits the tree. Code that walks the tree sees a
  `oneof` whose meaning depends on the profile.
- Why it lost: like B3's F, it trades a visible, one-time break for a permanent, silent one, and B3 (which E7 now
  depends on) already makes the next release 0.10.0. **If the owner ships B3 as F (0.9.x), E7 takes E**: it changes
  only plan steps 1, 4 and 5 (a marker instead of a class; the macros add the marker to the `oneof` they already emit).

### B. A compile-time flag `json.schema.Flag.EitherUntagged`  — LOST
Like `Flag.EnumsAsOneOf`: with an implicit flag in scope where `Json.schema` is called, the macros emit `oneof[L, R]`.
- Pros: small. No new node, no renderer or profile change.
- Cons: decided where the schema is derived, not where it is rendered. A schema shared by two services can't be
  rendered both ways. The bridge profiles can't set it. Every `Json.schema` call site needs the flag. It covers only
  one of the five encodings. This goes against #88: the profiles exist so that a library's encoding is a render-time
  choice.

### C. Emit `Either` as a `family` (members `Left`, `Right` of kind Product)  — LOST
With no new node, the existing family lowering would shape `Either` with the profile's `sumStyle`. Under Neutral/Legacy
(`Untagged`) that is today's output.
- Cons: it couples `Either` to `sumStyle`. A user profile `Neutral.withSum(Discriminator("type"))` would silently change
  its `Either` output. The play profile would render `{"_type": "scala.util.Left", "value": 1}`, which play doesn't
  write. It still can't express what spray, json4s and upickle write (they encode `Either` natively, not as a family).
  Only circe-generic writes `Either` as a family, and that case survives as `EitherStyle.AsFamily` under A.

### D. Documentation only: tell users to provide an implicit `Schema[Either[A, B]]`  — LOST
`implicit def eitherS[A, B](implicit a: Schema[A], b: Schema[B]): Schema[Either[A, B]] = Schema.`oneof`(Set(a, b))`
works today.
- Cons: by the core rule an implicit `Schema` becomes a definition (`$ref`) for every `Either` type. `A`/`B` also need
  implicit schemas (derived types need their own `implicit val`s). A `Predef[Either[A, B]]` can't be built for derived
  `A`/`B` (E1's problem). Nothing improves for bridge users. Still worth a README line as the workaround for a codec no
  style describes.

### Sub-decisions inside A

**A1. Default style: `ValueWrapped` everywhere except the bridge profiles that know better.** WON (the item says
"defaulting to today's output"). Neutral and Legacy keep `ValueWrapped`. Legacy must: it is 0.7's style. *Alternative
for the owner:* Neutral switches to `Untagged`, which is what a hand-written codec most likely writes and what #220
asked for. It is one line (`Knobs("neutral", ...).copy(eitherStyle = Untagged)`) and changes the Neutral rendered output
of every `Either` (golden `groupC/Either[Int,String]`, `groupC/HasEither`). LOST here because nothing tells us what the
user's codec writes, and Neutral has been "0.7 shapes, `Option` as nullable".

**A2. `Indexed` needs a positional array.** The model has no node for `[0, L]` (`TupleN` renders as an object
`{"_1", "_2"}`, BACKLOG 8).
- (a) **WON**: a new plain node `Schema.tuple(items: List[Schema[_]])` that renders as JSON Schema "tuple validation":
  drafts 04 to 2019-09 `{"type":"array","items":[...],"additionalItems":false,"minItems":n}`, 2020-12
  `{"type":"array","prefixItems":[...],"items":false,"minItems":n}`. It is small and reusable: a later
  "tuples as arrays" item (BACKLOG 8: circe, upickle, spray and play write tuples as arrays) can lower a Scala tuple to
  it. The implementation plan keeps it as separate steps (Part 2), so it can ship in a second PR.
- (b) LOST: defer `Indexed`, and the upickle profile keeps `ValueWrapped`. It is simpler, but upickle's own output
  stays rejected and the item asks to match each library.
- (c) LOST: approximate with `array(oneOf[enum[0], L])` plus `minItems`/`maxItems` 2. It accepts `[1, 0]` and `[0, 0]`,
  so it doesn't describe the encoding.

**A3. Is `ValueWrapped`'s `value` field required when its side is an `Option`? Lead's decision (2026-11-23): (ii), not
required everywhere. (i) and (iii) stay as the owner's alternatives.** Today
it is required on Scala 2 and not on Scala 3. Scala 2's `required` comes from `Left`'s field being typed as the
unsubstituted type parameter `A`, so the "an `Option` field isn't required" rule doesn't fire. That is arguably a 0.7
bug, not a 0.7 style (E7-R3). `AsFamily`'s members use the same field and follow the same answer. Three choices:
- (i) **required everywhere** (Scala 2 = 0.7). Only Scala 3 output changes: Neutral adds `"required": ["value"]` to the
  member; Legacy turns `value` into 0.7's `oneOf[{"value": X}, "None"]`. Legacy is byte-exact 0.7 on both versions. It
  changes the smaller (newer, since 0.8.0) user base and touches no Scala 2 output (my first recommendation).
- (ii) **not required everywhere** (the Option-field rule). **WON (lead).** Only Scala 2 output changes: Neutral drops
  `"required": ["value"]`, and Legacy switches to the dropped-layer field (`value: X`, not required). It is what the
  rule says for every other `Option` field. Scala 2's `required` is a 0.7 bug (the rule didn't fire on a type
  parameter), and the Legacy policy fixes bugs under Legacy too, with the change listed in the PR.
- (iii) **Legacy required, the other profiles not required.** 0.7 bytes under Legacy and the rule elsewhere. It
  changes Scala 2 Neutral and Scala 3 Legacy, and it couples the field to `optionStyle`.
No golden pins this case today. The new `EitherShapes` golden pins whichever is chosen, as one file for Scala 2 and 3.

**A4. An implicit `Schema`/`Predef` for `Left[A, B]` or `Right[A, B]` is no longer consulted.** WON: the macros resolve `L`
and `R` directly, like `Option`'s element. Nobody declares schemas for `Left`/`Right`. Someone who did can declare
`Schema[Either[A, B]]` instead, which still wins (core rule, step 1). *Alternative:* fall back to today's plain
`oneof` when such an implicit exists. That keeps a second `Either` code path alive in both macros for a case nobody
has.

**A5. Overlapping sides: B3's rule, from the start (E7-R1).** An exclusive `oneOf` of overlapping sides rejects values
the libraries write. On networknt draft-07 and 2020-12 (r-e7's probe), `Either[String, UUID]` + a UUID,
`Either[String, LocalDate]` + `"2024-01-02"`, `Either[Int, Long]` + `5` (Long is `number` until D2) and
`Either[String, Color]` (an enum) + `"Red"` are all invalid ("2 are valid"). These are common `Either` shapes, and
`Untagged` would make them the default for json4s and spray. So E7 is built on B3 (#470). The styles that build a new
`oneof` (`Untagged`, `WrapperObject`, `Indexed`) pass it through B3's disjointness check, and with
`overlappingMembers = AnyOf` (B3's default, every built-in profile) sides that can't be proven disjoint become
`anyof`. `OneOf` keeps `oneOf`. By style:
- `Untagged`: `oneOf` when B3 proves the sides disjoint (`Either[Int, String]`, `Either[Foo, Bar]` with distinct required
  fields), otherwise `anyOf`. `anyOf` always states its type (B3), which also fixes the root case.
- `WrapperObject`: always `oneOf` (B3's key rule: one required key per closed object).
- `Indexed`: B3's check doesn't know `tuple`, so it says "overlap" and the result is `anyOf`. That is still exact,
  because the first items' one-value enums `0`/`1` never both match, and `anyOf` states `"type": "array"` at the root.
  Teaching the check a "(tuple) disjoint when some position is disjoint" rule would give `oneOf`. It is not needed,
  and it is left out to keep B3's check unchanged.
- `AsFamily`: `familyAt` does it (B3 applies its rule to untagged families; tagged ones stay `oneOf`).
- `ValueWrapped` (and so Neutral and Legacy): today's plain `oneof`, unchanged. Whether it goes through the check too
  (`Either[Int, Double]`'s `{"value": 1}` is rejected today) is B14's call. It is one call in `eitherAt`. With B3 merged,
  `AsFamily` under an untagged `sumStyle` may then be `anyOf` where `ValueWrapped` is `oneOf`, and B14 should align them.

**Spray gets `anyOf`** (the default knob), not `oneOf`. spray's reader rejects an ambiguous value, but its writer still
writes one (`Left("2024-01-02")` as `"2024-01-02"`), and a profile describes what the library writes. Getting `oneOf`
through `SprayProfile.default.withOverlappingMembers(OneOf)` would also change spray's families (B3's knob is per
profile, not per node), for an exactness only spray's reader needs. A user who wants it sets the knob.

## Decision

A: a new `Schema.either(left, right)` node recorded by both macros, lowered by a new profile knob
`eitherStyle: EitherStyle` (`ValueWrapped` default = today, `Untagged`, `AsFamily`, `WrapperObject(l, r)`, `Indexed`).
The bridge profiles describe their library: spray and json4s `Untagged`, upickle `Indexed` (on a new positional-array
node `Schema.tuple`), circe `AsFamily`. play keeps `ValueWrapped` (play has no `Either` format). Built on B3: the new
`oneof`s go through B3's overlap check, so overlapping untagged sides are `anyOf`. Neutral and Legacy output doesn't
change, except the `Either[Option[X], _]` parity fix (A3 (ii), the lead's call: `value` not required, so Scala 2
joins Scala 3). Breaking under PVP (0.10.0, with B3), or
E for a 0.9.x patch.

## Implementation plan

Part 1: the node, the knob, four styles (everything except `Indexed`). Part 2: `Schema.tuple` + `Indexed` + the upickle
profile. Both parts can go in one PR, or Part 2 in a second one. Base: B3's branch (#470), or `master` once B3 has
merged. Run `clean` before testing (macro changes).

### Part 1

1. **Model** (`core/src/main/scala/json/Schema.scala`): add, next to `option`,
   ```scala
   final case class `either`[L, R](left: Schema[L], right: Schema[R]) extends Schema[Either[L, R]]
   ```
   with `mkCopy`, `canEqual`, `equals` (both sides + `super.equals`), `jsonType` = `JsonSchemaProfile.Neutral.lower(this).jsonType`
   (as `family`), `toString` `either(L, R)`. Add a comment block in the style of `option`'s ("The Scala type here is
   `Either[L, R]`... lowered by a profile"). Update the `family` comment ("`Either` ... are not families": now `either`).
   `deepCopy` (~L584): `case `either`(l, r) => `either`[Any, Any](deepCopy(l)..., deepCopy(r)...)`. Update the
   `withProfile` scaladoc ("its `option` nodes") to name `option`, `family` and `either`.
2. **Profile** (`core/src/main/scala/json/schema/JsonSchemaProfile.scala`):
   - `EitherStyle` ADT as in option A, with scaladoc naming the library each style is for (as `SumStyle` does).
   - `def eitherStyle: EitherStyle = EitherStyle.ValueWrapped` (concrete, so user subclasses of the trait still compile),
     `final def withEither(x: EitherStyle)`, a `Knobs` field `eitherStyle: EitherStyle = EitherStyle.ValueWrapped`,
     `Knobs.apply(p)` and `Knobs.profile` updated. The trait's scaladoc lists the third fact ("this is an `Either`").
   - `Lowering.apply`: `case e: `either`[_, _] => eitherAt(e).withExtraFrom(s)` (the field's title/description stay on the
     outer node, as for `option`).
   - `eitherAt(e)`. The wrapping styles build their objects around the **unlowered** sides and then `apply` the
     object, so each side goes through `field(...)` as a derived field would (a Legacy `option` side lowers as the
     Scala 3 derivation's `Left` object does). `Untagged` and `Indexed` lower the sides directly (`apply(e.left)`). The
     `value` field's `required` follows A3 (ii): `required = !side.isInstanceOf[`option`[_]]`, the rule the macros use for
     any `Option` field. (Under (i) it would be `true`; under (iii) `true` only when `p.optionStyle == Legacy`.) `Untagged`, `WrapperObject` and `Indexed` finish with B3's overlap step:
     `inclusiveIfOverlapping(members)`, i.e. the same private helper `familyAt` uses for untagged families (factor it
     out of `familyAt` if B3 inlined it), honoring `p.overlappingMembers` (A5):
     - `ValueWrapped`: `oneof(ListSet(apply(obj(Field("value", e.left, required = A3))), apply(obj(Field("value", e.right, required = A3)))), None)`,
       objects built with `object.fromList`. Member order Left, Right (a 2-element `Set` iterates in insertion order, as
       `optionAt` relies on).
       (B3 makes the `oneof`/`anyof` members `ListSet`s; on today's `Set` a 2-element set iterates in insertion order too).
     - `Untagged`: overlap step over `List(apply(e.left), apply(e.right))`. Edge: for `Either[X, X]` the sides are equal:
       render `X` itself (one entry, as B3 keeps exact twins as one entry). Today's `ValueWrapped` collapses the same way
       (`oneOf[{"value": X}]`, a single member). Note it in the scaladoc.
     - `WrapperObject(lk, rk)`: overlap step over the two wrapper objects (always disjoint by B3's key rule, so `oneOf`).
       The case class rejects `lk == rk` (`require`, the sides couldn't be told apart). Both wrapper keys are required,
       whatever the side: `encodeEither` always writes the key (`{"Left": null}` for an `Option` side).
     - `AsFamily`: `familyAt(family(List(member("Left", "scala.util.Left", classOf[Left[_, _]], e.left), member("Right", ...)),
       None, enumsAsOneOf = false))`. Members are `Kind.Product` with schema `obj(Field("value", side, required = A3))`
       (the same `value` field as `ValueWrapped`; unlowered: `familyAt` lowers each member's schema itself), `sig` = the full name (only used under a honored
       `@discriminator`, which `Either` can't carry), no texts. Under Untagged sum profiles this equals `ValueWrapped`.
   - Idempotence: a lowered tree has no `either` left (`lower` stays idempotent). Extend the `lower` scaladoc.
3. **Core tree tools**: `AsTree.scala` (`case `either`(l, r) => Apply("either", Iterator(AsTree(l), AsTree(r)))`),
   `SchemaEquality.scala` (pairwise like `option`, ~L101), `DefinitionNames.scala` (`signatures(l) ++ signatures(r)`,
   ~L59), `AsDraftSupport.lowered` comment ("`option`, `family` or `either` nodes left").
4. **Scala 2 macro** (`macros/src/main/scala-2/...`):
   - `SchemaTypes.scala`: `case class EitherOf(tpe: Type, leftTpe: Type, rightTpe: Type, left: SchemaType, right: SchemaType, extra: Extra = Extra())`
     (not `Either`: it would shadow `scala.Either` wherever the cake imports `U._`, e.g. `ScalaParser`, `USumTypes`)
     with `prefix = q"${N.Schema}.`either`[$leftTpe, $rightTpe](${left.tree}, ${right.tree})"`. Add it to
     `transformSchema` (~L320) next to `Opt`, and wherever `Opt`/`OneOf` are walked (grep `case .*Opt(` /
     `OneOf(` in `macros/`: recursion/def wrapping, `withTypeAnnotations`).
   - `Macroses.genTree` (~L136): right after the `Option` case,
     `case t if t.typeSymbol == typeOf[scala.util.Either[_, _]].typeSymbol && t.typeArgs.size == 2 => U.EitherOf(t, l, r, resolve(l, ctx :+ t), resolve(r, ctx :+ t))`
     (`tpe` is dealiased already, so `scala.Either` works too).
   - `USumTypes.scala`: drop `isEither` and `eitherOneOf` (Either never reaches `SumType` now).
5. **Scala 3 derivation** (`core/src/main/scala-3/.../derivation/`):
   - New `Eithers.scala` (one file per concern): `final case class EitherNode(tpe, left: Node, right: Node, extra)`.
     `children = List(left, right)`, `mapChildren`, and `emitRaw` emits `Schema.`either`[l, r](...)` through
     `left.tpe.asType` / `right.tpe.asType`. `deriveEither(tpe, ctx)`: `classSymbol(tpe) == TypeRepr.of[Either[Any, Any]].typeSymbol`,
     sides from `tpe.baseType(EitherSym).typeArgs`, each `resolve(_, ctx.descend(tpe))`. Mix it into `Derivation`.
   - `Resolve.families`: insert `deriveEither` right after `deriveOption`. Update the comment's list.
   - `Sums.scala`: remove `SumKind` (only `Sealed` would be left: drop the field) and `emitSum` with its `Entry`. They are
     used only for `Either`: check with grep, and `FamilyEmit.scala`'s comment mentions `emitSum`. Update the trait
     scaladoc ("and `Either`").
   - Check the recursion pass (notes/reports/skeleton-ir.md) handles the node through `children`/`mapChildren` (a
     recursive `Either[String, Node]` must give the `ref` as today: covered by a test below).
6. **Bridge profiles** (each `*Profile.scala` + its scaladoc; README table):
   | profile | style | rendered output change |
   |---|---|---|
   | `SprayProfile.default` | `Untagged` (`DefaultJsonProtocol.eitherFormat`) | yes |
   | `Json4sProfile.fromFormats(f)` | `Untagged` (Extraction writes `Either` untagged, hints or not) | yes |
   | `CirceProfile.configured(...)` / `default` / Scala 3 `fromConfig` | `AsFamily` (circe-generic auto; under `configured` the family shape follows the discriminator/constructor names, as generic-extras auto derives it) | yes |
   | `UPickleProfile.fromApi(api)` | Part 2: `Indexed`; until then unchanged | Part 2 |
   | `PlayProfile.fromConfig(cfg)` | `ValueWrapped` (play has no `Either` format: the user's own) | no |
7. **Tests** (shared `src/test/scala` unless noted):
   - Golden: the tree sections of `golden/groupC/Either[Int,String].json` and `golden/groupC/HasEither.json` change
     (`"kind": "either"` with `left`/`right`). Their rendered sections stay the same. `golden-legacy/` stays the same (the
     Legacy check compares the lowered tree). `harness/Golden.scala` tree dump: an `"either"` kind
     (`"left"`, `"right"`). `harness/Families.scala`: walk `either`'s sides in `walk`/`map` (keep the node, as for `option`).
   - New golden model `groupC/EitherShapes` (fields: `Either[Option[Int], String]`, nested, `List[Either]`,
     `Option[Either]`, a side with an implicit `Schema`, a recursive `Either[String, Node]`). Update mode adds
     `golden/` and the missing `golden-legacy/` files. Scala 2 and 3 must produce one file, which pins A3's parity fix.
   - A new `api/src/test/scala/.../profiles/EitherStyleSpec.scala`: for `Either[Int, String]` and a field of it, each
     style's lowered tree (`withProfile(Neutral.withEither(x))`) and its draft-07 / 2020-12 JSON; `withEither` keeps
     the other knobs (and `with...` keeps `eitherStyle`); `WrapperObject("k", "k")` throws;
     `AsFamily` under `Neutral.withSum(Discriminator("t"))` gives `{"t": "scala.util.Left", "value": ...}`; lowering is
     idempotent; `Legacy.lower` equals today's tree. Validated with networknt on 04/07/2020-12, not only compared as
     trees (E7-R7):
     - overlap (E7-R1), under `Untagged`: `Either[String, UUID]` + a UUID, `Either[String, LocalDate]` + `"2024-01-02"`,
       `Either[Int, Long]` + `5`, `Either[String, Color]` (a case-object enum) + `"Red"`: valid (`anyOf`); with
       `withOverlappingMembers(OneOf)`: invalid (pins the knob). `Either[Int, String]` stays `oneOf`;
     - root (E7-R5): a root `Either[Foo, Bar]` (objects) under every style rejects `5`, `"s"` and `null`, and accepts its
       own documents. The doc states the keyword per style: A5;
     - `Option[Either[Int, Long]]` under `Untagged` (nullable over an `anyOf`: B3's `mkNullable` path) accepts `5`,
       `null`, and rejects `"s"`;
     - `Either[String, String]` under `Untagged` is `{"type": "string"}` itself and accepts `"a"`.
   - Acceptance (each bridge module's petshop spec): the library's real `Either` output (root and field) validates
     under its profile on 04/07/2020-12, and another style's output doesn't. spray: `DefaultJsonProtocol` (2.12/2.13/3),
     also `Either[Int, Long]` with `Right(5L)` (written as `5`, valid under the `anyOf`, A5). json4s (Scala 2 dir, as
     today): `DefaultFormats` and `ShortTypeHints`, also `Either[String, Int]`/`Either[Int, Long]` overlap documents. circe: `generic.auto._` (2.12/2.13/3),
     generic-extras `auto._` with a discriminator (Scala 2 dir), and an `encodeEither` user with
     `CirceProfile.default.withEither(WrapperObject("Left", "Right"))`. upickle (Part 2). play: no codec, so assert the
     unchanged output. Add the new docs to `LibraryOutputs` only if the cross-check uses them (it is about `Pet`; leave
     it).
   - Scala.js: nothing platform-specific. The validating tests cancel on JS as usual. Tree and JSON tests run there.
8. **Docs** (README and paradox say the same):
   - README "Library profiles": replace the bullet "`Either` is not a family (still the 0.7 `oneOf`)" with how `Either`
     is shaped (the style per profile). Add a row to the knob table:
     `withEither | EitherStyle.ValueWrapped, Untagged, AsFamily, WrapperObject(left, right), Indexed | how an Either is written`.
     The library table can name each library's `Either` encoding in the "as it writes" column or a sentence below it.
     circe (E7-R6): one line saying that `AsFamily` is what `generic.auto` writes. With semiauto there is no
     `Encoder[Either]`, and a user's `Encoder.encodeEither(l, r)` is
     `CirceProfile.default.withEither(EitherStyle.WrapperObject(l, r))`. Another line says that overlapping untagged sides
     render `anyOf` (B3's knob).
   - README "Known issues": drop the `Either` half of the tuples line (tuples stay). Add the workaround (D) for a codec no
     style describes: an implicit `Schema[Either[A, B]]`.
   - `docs/src/main/paradox/rendering.md`: the same two changes (~L180 bullet, knob table ~L191). Run `sbt makeDocs`.
   - Scaladoc of `JsonSchemaProfile`, `Schema.either`, each profile. CHANGELOG: the lead's.

### Part 2 (`Indexed`, upickle)

9. **Model**: `final case class `tuple`[T](items: List[Schema[_]]) extends Schema[T]`: a positional array. `jsonType = "array"`,
   `deepCopy`, `toString` `tuple(a, b)`. Scaladoc: "an array whose i-th element is `items(i)` and nothing more (JSON
   Schema tuple validation). The derivation doesn't emit it (Scala tuples are still objects); `EitherStyle.Indexed` does."
10. **Render** (`AsDraftSupport.scala`, near `array` ~L125): drafts 04/06/07/2019-09 render
    `"items": [..each with includeType = true..], "additionalItems": false, "minItems": n`. 2020-12 overrides a hook
    (`protected def tupleItems(items: List[obj]): obj`) in `AsDraft12` to `"prefixItems": [...], "items": false, "minItems": n`.
    Add it to `memberType`/`jsonType` paths (it is `"array"`), `inferDefinitions` (definitions inside items), `AsTree`,
    `SchemaEquality`, `DefinitionNames`, `Lowering.apply` (walk the items), the harness tree dump (`"tuple"`).
11. **Indexed lowering**: the overlap step over `List(tuple(List(enum(integer, Set(num(0))), apply(e.left))), tuple(List(enum(integer, Set(num(1))), apply(e.right))))`.
    B3's check doesn't know `tuple`, so the result is `anyof` (exact, see A5).
    Use a one-value `enum`, not `const`, because draft-04 has no `const` (the renderer writes it anyway, which is a
    known issue).
12. **upickle profile**: `UPickleProfile.fromApi` sets `Indexed` (every `AttributeTagged` API shares upickle's
    `Either` reader/writer, so it doesn't depend on the API's config: test with the README's `MyPickler`).
13. **Tests**: `AsDraft04Spec`/`06`/`07`/`09`/`12Spec`: full JSON of a `tuple` (with a `$ref` item). `EitherStyleSpec`:
    `Indexed` on every draft. upickle acceptance: `[0,1]`, `[1,"a"]`, `{"e":[0,1]}` valid on 04/07/2020-12, `[1,1]`
    and `[0,"a"]` invalid. networknt supports `prefixItems`, `items: false` (2020-12) and `additionalItems` (older).
    The test confirms it.

### Risks
- Depends on B3 (#470). If B3's design changes (e.g. the owner picks its marker variant F), E7 follows: option E, and
  `eitherAt` calls whatever overlap step B3 ships. If B3 is dropped, E7 must not ship `Untagged` as a plain `oneOf`
  (E7-R1). It would then need its own small overlap rule, and that is a re-review.
- Merge conflicts with neighbours that touch the same matches and `Knobs`: E2 (`any` node), B3 (`anyof`,
  `OverlappingMembers` knob), D4 (field-naming knob). They are mechanical. Rebase onto whichever lands first.
- A missed walker in the Scala 2 macro (a `SchemaType` match without the new case) shows up as a `MatchError` at
  compile time of the tests. Grep every `case .*Opt(` and `case .*OneOf(` in `macros/`.
- `AsFamily` for circe configured (generic-extras auto with a discriminator): the plan asserts it with a test. If the
  library writes something else, the profile uses the matching style, and the doc + review log get updated.

## Compatibility

- **Source**: additive. A new case `either` (Part 2: `tuple`) in the sealed `Schema`: a user's exhaustive `match` gets a
  non-exhaustive warning (an error only under `-Xfatal-warnings`), as `option`/`family` did in 0.8/0.9 and E2's `any`
  will. New `EitherStyle`, `eitherStyle` (concrete default) and `withEither`. A user's own `JsonSchemaProfile` subclass
  still compiles. An implicit `Schema[Left[..]]`/`Schema[Right[..]]` is no longer used (A4). Nobody is known to have one.
- **Binary**: new classes and members only (a trait method with a body is an interface default method on 2.12+ and 3).
- **Output**:
  - Neutral and Legacy: rendered output unchanged, except `Either` with an `Option` side on Scala 2 (A3 (ii): `value`
    no longer required; Legacy: the dropped-layer `value: X`). The derived tree changes (`either` node) for every `Either`: visible in
    `toString`, `AsTree`, the golden tree sections and any user code that walks derived trees, not in rendered JSON.
  - Bridge profiles: `Either` changes under spray, json4s, circe (Part 1) and upickle (Part 2), to what each library
    writes. Overlapping untagged sides render `anyOf` (A5). play: unchanged.
- **PVP: breaking, 0.10.0** (E7-R2). There are three reasons: (a) a new case in the exported sealed `Schema` (as B3
  judged its `anyof`, B3-R1); (b) `Json.schema[Either[A, B]]` returns a different node for every existing `Either`, so
  code that matches derived trees changes behaviour silently; (c) the default output of 3 (Part 2: 4) bridge
  profiles changes. The team is split on (a): E2's doc calls a new node 0.9.x. The lead or owner should rule once for
  B3, E2, E6 and E7. For a 0.9.x patch, E (a marker on `oneof`) removes (a) and (b). (c) remains: a fix (today's schema
  rejects all of the library's own output), but visible. E7 depends on B3, so it ships in B3's release in any case.
- **Legacy**: 0.7 output byte for byte for every existing golden. `golden-legacy/` is unchanged (new files only for
  the new model). The `Either[Option[X], _]` case follows A3 (ii): Scala 2's Legacy output changes as a bug fix, which
  the lead's policy allows when listed. No existing golden-legacy file has this case.
- **Core rule**: unchanged. An implicit `Schema[Either[A, B]]` / `Predef[Either[A, B]]` wins before any of this. `L` and
  `R` follow the rule like any type.

## Review log
Round 1 (r-e7, on 1fa81eb):
- E7-R1 (must) `Untagged` as an exclusive `oneOf` rejects overlapping sides the libraries write; build on B3 →
  accepted: the doc depends on #470. `Untagged`/`WrapperObject`/`Indexed` go through B3's overlap check (A5, plan
  step 2). Spray gets `anyOf` (its writer writes ambiguous values; B3's knob is per profile, so `OneOf` is the user's
  choice). `ValueWrapped`/Legacy are left to B14. The four overlap cases and spray/json4s overlap documents are in the
  tests. Decision, Risks and Compatibility are updated.
- E7-R2 (must) one PVP verdict → accepted: breaking, 0.10.0, for (a) the new case, (b) a different derived node, (c)
  the bridge output. The B3/E2 split is noted for the lead/owner to rule once. The patch alternative is the new option E
  (a marker on `oneof`, as B3's F), and it is an open question for the PR.
- E7-R3 (must) A3 is a side-effect output change, needs the lead; Scala 2's `required` is a 0.7 bug → accepted: three
  choices presented and mailed to the lead. The lead chose (ii), not required everywhere; (i)/(iii) are kept as owner
  alternatives. Plan step 2 and Compatibility are updated.
- E7-R4 (nit) `U.Either` shadows `scala.Either` → accepted: `U.EitherOf`.
- E7-R5 (nit) root position, keyword per style → accepted: the keyword per style is in A5; the root tests reject `5`,
  `"s"` and `null` under every style. `Indexed` gets `anyOf` (B3's check doesn't know `tuple`; exact anyway).
- E7-R6 (nit) README line for circe semiauto + `encodeEither` → accepted (plan step 8).
- E7-R7 (nit) `Option[Either]` and `Either[X, X]` under `Untagged`, validated → accepted (plan step 7).
