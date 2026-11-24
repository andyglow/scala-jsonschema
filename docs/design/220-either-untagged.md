# `Either` shaped by the profile: an `eitherStyle` knob  (issue #220, item E7)
Status: draft

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
| `Untagged` | `{"oneOf":[{"type":"integer"},{"type":"string"}]}` |
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
  as for `option`/`family`/E2's `any`). `Indexed` needs a positional-array node that the model doesn't have (sub-decision
  A2).

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

**A3. `ValueWrapped`'s `value` field is always required**, as on Scala 2 (0.7). WON: Legacy is the 0.7 output, and 0.7
was Scala 2. This changes Scala 3's `Either[Option[X], _]` / `Either[_, Option[X]]` to match Scala 2 (Neutral: the
member gets `"required": ["value"]` back; Legacy: `value` becomes 0.7's `oneOf[{"value": X}, "None"]`). *Alternative:*
follow Scala 3 (not required) on both versions. That changes Scala 2/Legacy output, which the Legacy policy forbids
without the lead.

**A4. An implicit `Schema`/`Predef` for `Left[A, B]` or `Right[A, B]` is no longer consulted.** WON: the macros resolve `L`
and `R` directly, like `Option`'s element. Nobody declares schemas for `Left`/`Right`. Someone who did can declare
`Schema[Either[A, B]]` instead, which still wins (core rule, step 1). *Alternative:* fall back to today's plain
`oneof` when such an implicit exists. That keeps a second `Either` code path alive in both macros for a case nobody
has.

**A5. Overlapping sides are not decided here.** `Untagged` renders `oneOf` (today's keyword).
`Either[Int, Double]` then rejects `1` (it is an `integer` and a `number`). That is right for spray (it rejects
ambiguous values itself) and wrong for json4s (first side wins). `ValueWrapped` has the same problem today
(`{"value": 1}`). It is B14's (after B3's `anyof` node and its `OverlappingMembers` knob): once B3 is in, `eitherAt`
passes its `oneof` through B3's overlap check like a family (one call). The other styles are tagged, so their sides
never overlap.

## Decision

A: a new `Schema.either(left, right)` node recorded by both macros, lowered by a new profile knob
`eitherStyle: EitherStyle` (`ValueWrapped` default = today, `Untagged`, `AsFamily`, `WrapperObject(l, r)`, `Indexed`).
The bridge profiles describe their library: spray and json4s `Untagged`, upickle `Indexed` (on a new positional-array
node `Schema.tuple`), circe `AsFamily`. play keeps `ValueWrapped` (play has no `Either` format). Neutral and Legacy
output doesn't change; Scala 3 joins Scala 2 for `Either[Option[X], _]`.

## Implementation plan

Part 1: the node, the knob, four styles (everything except `Indexed`). Part 2: `Schema.tuple` + `Indexed` + the upickle
profile. Both parts can go in one PR, or Part 2 in a second one.
Run `clean` before testing (macro changes).

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
     Scala 2 `Left` object did, A3). `Untagged` and `Indexed` lower the sides directly (`apply(e.left)`):
     - `ValueWrapped`: `oneof(Set(apply(obj(Field("value", e.left, required = true))), apply(obj(Field("value", e.right, required = true)))), None)`,
       objects built with `object.fromList`. Member order Left, Right (a 2-element `Set` iterates in insertion order, as
       `optionAt` relies on).
     - `Untagged`: `oneof(Set(apply(e.left), apply(e.right)), None)`. Edge: for `Either[X, X]` the `Set` has one element:
       render `X` itself. Today's `ValueWrapped` collapses the same way (`oneOf[{"value": X}]`, a single member, B3's
       report). B3's ordered set changes the `oneof` side of that, not this one. Note it in the scaladoc.
     - `WrapperObject(lk, rk)`: `oneof(Set(apply(obj(Field(lk, e.left, true))), apply(obj(Field(rk, e.right, true)))), None)`.
       The case class rejects `lk == rk` (`require`, the sides couldn't be told apart).
     - `AsFamily`: `familyAt(family(List(member("Left", "scala.util.Left", classOf[Left[_, _]], e.left), member("Right", ...)),
       None, enumsAsOneOf = false))`. Members are `Kind.Product` with schema `obj(Field("value", side, required = true))`
       (unlowered: `familyAt` lowers each member's schema itself), `sig` = the full name (only used under a honored
       `@discriminator`, which `Either` can't carry), no texts. Under Untagged sum profiles this equals `ValueWrapped`.
   - Idempotence: a lowered tree has no `either` left (`lower` stays idempotent). Extend the `lower` scaladoc.
3. **Core tree tools**: `AsTree.scala` (`case `either`(l, r) => Apply("either", Iterator(AsTree(l), AsTree(r)))`),
   `SchemaEquality.scala` (pairwise like `option`, ~L101), `DefinitionNames.scala` (`signatures(l) ++ signatures(r)`,
   ~L59), `AsDraftSupport.lowered` comment ("`option`, `family` or `either` nodes left").
4. **Scala 2 macro** (`macros/src/main/scala-2/...`):
   - `SchemaTypes.scala`: `case class Either(tpe: Type, leftTpe: Type, rightTpe: Type, left: SchemaType, right: SchemaType, extra: Extra = Extra())`
     with `prefix = q"${N.Schema}.`either`[$leftTpe, $rightTpe](${left.tree}, ${right.tree})"`. Add it to
     `transformSchema` (~L320) next to `Opt`, and wherever `Opt`/`OneOf` are walked (grep `case .*Opt(` /
     `OneOf(` in `macros/`: recursion/def wrapping, `withTypeAnnotations`).
   - `Macroses.genTree` (~L136): right after the `Option` case,
     `case t if t.typeSymbol == typeOf[scala.util.Either[_, _]].typeSymbol && t.typeArgs.size == 2 => U.Either(t, l, r, resolve(l, ctx :+ t), resolve(r, ctx :+ t))`
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
     the other knobs (and `with...` keeps `eitherStyle`); `WrapperObject("k", "k")` throws; `Untagged` of `Either[X, X]`;
     `AsFamily` under `Neutral.withSum(Discriminator("t"))` gives `{"t": "scala.util.Left", "value": ...}`; lowering is
     idempotent; `Legacy.lower` equals today's tree.
   - Acceptance (each bridge module's petshop spec): the library's real `Either` output (root and field) validates
     under its profile on 04/07/2020-12, and another style's output doesn't. spray: `DefaultJsonProtocol` (2.12/2.13/3).
     json4s (Scala 2 dir, as today): `DefaultFormats` and `ShortTypeHints`. circe: `generic.auto._` (2.12/2.13/3),
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
11. **Indexed lowering**: `oneof(Set(tuple(List(enum(integer, Set(num(0))), apply(e.left))), tuple(List(enum(integer, Set(num(1))), apply(e.right)))), None)`.
    Use a one-value `enum`, not `const`, because draft-04 has no `const` (the renderer writes it anyway, which is a
    known issue).
12. **upickle profile**: `UPickleProfile.fromApi` sets `Indexed` (every `AttributeTagged` API shares upickle's
    `Either` reader/writer, so it doesn't depend on the API's config: test with the README's `MyPickler`).
13. **Tests**: `AsDraft04Spec`/`06`/`07`/`09`/`12Spec`: full JSON of a `tuple` (with a `$ref` item). `EitherStyleSpec`:
    `Indexed` on every draft. upickle acceptance: `[0,1]`, `[1,"a"]`, `{"e":[0,1]}` valid on 04/07/2020-12, `[1,1]`
    and `[0,"a"]` invalid. networknt supports `prefixItems`, `items: false` (2020-12) and `additionalItems` (older).
    The test confirms it.

### Risks
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
  - Neutral and Legacy: rendered output unchanged on Scala 2. On Scala 3, `Either` with an `Option` side gets
    `value` required, as on Scala 2 (A3). The derived tree changes (`either` node) for every `Either`: visible in
    `toString`, `AsTree` and the golden tree sections, not in rendered JSON.
  - Bridge profiles: `Either` changes under spray, json4s, circe (Part 1) and upickle (Part 2), to what each library
    writes. play: unchanged.
- **PVP**: the API is additive (0.9.x). The bridge profiles' output change is a fix (today's schema rejects all of the
  library's own output) but still a visible change. The owner decides whether that fits a 0.9.x patch or waits for
  0.10.0 with B3/E2.
- **Legacy**: 0.7 output byte for byte; `golden-legacy/` unchanged (new files only for the new model).
- **Core rule**: unchanged. An implicit `Schema[Either[A, B]]` / `Predef[Either[A, B]]` wins before any of this. `L` and
  `R` follow the rule like any type.

## Review log
