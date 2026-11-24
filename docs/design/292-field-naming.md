# Field naming in profiles  (issue #292, closed #270, item D4)
Status: draft

## Problem

A profile describes how a JSON library shapes `Option`s and sealed families, but not field names. When the library
renames fields (play `JsonConfiguration.naming`, circe `transformMemberNames`, upickle `objectAttributeKeyWriteMap`,
PureConfig's kebab-case keys), the schema keeps the Scala names, so it rejects the library's own output. The 0.9.0
bridge profiles detect the renaming (on sample names) and throw instead of producing that schema.

Repro (play-json, the same on 2.12.21, 2.13.18 and 3.3.9; source in `notes/scratch/c-d4/D4ReproSpec.scala`, run as
`play-json/testOnly petshop.D4ReproSpec`):

```scala
final case class Order(orderId: String, itemCount: Int, giftNote: Option[String], line: OrderLine)
final case class OrderLine(unitPrice: Double, skuCode: String)

implicit val cfg: JsonConfiguration    = JsonConfiguration(JsonNaming.SnakeCase)
implicit val lineF: OFormat[OrderLine] = Json.format[OrderLine]
implicit val orderF: OFormat[Order]    = Json.format[Order]
```

What play writes:

```json
{"order_id":"o-1","item_count":2,"gift_note":"hi","line":{"unit_price":9.5,"sku_code":"X1"}}
```

`Json.schema[Order].asPlay(Draft07(...))` with `AsPlay._` (Neutral):

```json
{"type":"object","additionalProperties":false,
 "properties":{"orderId":{"type":"string"},"itemCount":{"type":"integer"},"giftNote":{"type":["string","null"]},
   "line":{"type":"object","additionalProperties":false,
     "properties":{"unitPrice":{"type":"number"},"skuCode":{"type":"string"}},"required":["unitPrice","skuCode"]}},
 "required":["orderId","itemCount","line"]}
```

networknt (draft-07) rejects play's document with 9 errors (`required property 'orderId' not found`,
`property 'order_id' is not defined in the schema and the schema does not allow additional properties`, ...).
With the library's profile it fails earlier, when the profile is built:

```
PlayProfile.fromConfig(cfg)
IllegalArgumentException: PlayProfile: JsonConfiguration.naming renames fields (fooBar -> foo_bar), which a
JsonSchemaProfile can't describe. Use JsonNaming.Identity, or give the types an implicit json.Schema of your own
```

`CirceProfile.configured(transformMemberNames = ...)`, Scala 3 `CirceProfile.fromConfig(Configuration.default.withSnakeCaseMemberNames)`
and `UPickleProfile.fromApi(api)` with a renaming `objectAttributeKeyWriteMap` throw the same way (tests:
`CirceAcceptanceSpec` "fail fast on transformMemberNames", `Circe3Spec`, `CirceExtrasSpec`, `PlayAcceptanceSpec`
"fail fast on a field renaming", `UPickleAcceptanceSpec`). Neither Neutral nor any knob can say "the keys are
snake_case", so a user of a renaming config has no profile at all.

The only workaround today is the one in #292: `Json.objectSchema[User]().withFieldsUpdated { case f => f.copy(name = kebab(f.name)) }`.
It renames the top-level object only: nested case classes, definitions and family members keep their Scala names.

## Why it is a problem

- snake_case (play `JsonNaming.SnakeCase`, circe `withSnakeCaseMemberNames`) and kebab-case (PureConfig's default)
  are common configurations. Three users asked (#270, closed as not planned in 0.7, and #292); #292's sum part is
  already possible with knobs (`withSum(Discriminator("type")).withMemberName(Custom(kebab)).withSingleton(AsTagged)`),
  the keys are what's left.
- The profiles' promise (README "Library profiles": "a setting the profile can't describe is an error ... not a
  schema that rejects the library's own output") holds, but the error is the end of the road: there is no
  supported way to get a correct schema for such a config short of hand-writing every object's `Schema`.

## Where the fix goes

Two questions decide the design:

1. **When** is the name changed: at derivation (the macro reads a naming config) or when the schema is lowered by a
   profile (like `Option` and families)?
2. **Which** fields does a profile rename: every field of every `object`, or only the fields the derivation took from
   a case class's constructor parameters?

The JSON libraries rename exactly the second set: the keys their derived codecs write for case-class parameters.
They don't touch keys of a hand-written codec, map keys, the discriminator field (play `discriminator`, circe
`discriminator`, upickle `tagName` are used as given) or the wrapper key of circe's sum encoding (that's
`transformConstructorNames`).

## Options

### A. A `fieldName` knob, applied at lowering to the fields the derivation named  — WON

A new profile knob, built like `memberName`:

```scala
// json.schema.JsonSchemaProfile
/** The JSON name of a field the derivation took from a case class parameter, from its Scala name. */
def fieldName: FieldName = FieldName.Identity
final def withFieldName(x: FieldName): JsonSchemaProfile = Knobs(this).copy(fieldName = x).profile

sealed trait FieldName { def apply(scalaName: String): String }
object FieldName {
  /** The Scala name (0.7, every built-in profile without a renaming config). */
  case object Identity extends FieldName { def apply(n: String): String = n }
  /** Any function of the Scala name: play's `cfg.naming`, circe's `transformMemberNames`, PureConfig's `ConfigFieldMapping(CamelCase, KebabCase)`. Profiles built with it don't compare equal. */
  final case class Custom(f: String => String) extends FieldName { def apply(n: String): String = f(n) }
}
```

The tree records which fields carry a Scala name, as a fact on the `object` node that isn't rendered (like
`Field.hasDefault`):

```scala
// json.Schema.`object`
/** The names of the fields the derivation took from case class parameters and that still have that name. A profile's
  * `fieldName` renames these when it lowers the schema. Not rendered, not part of equality. */
def scalaNamedFields: Set[String]
def withScalaNamedFields(names: Set[String]): `object`[T]
```

- Both derivations (Scala 2 macro, Scala 3 quotes, `derives json.Schema`, `Json.objectSchema`) set it to every field
  of a case class (not of a tuple: libraries write tuples as arrays, an unrelated mismatch, and `_1` is no one's
  config key).
- Every copy of an `object` (`rebuild`, `strict`, `Free.of`, so `withField`, `dropField`, `withFieldsUpdated`,
  `duplicate`, `toDefinition`) keeps the set, intersected with the copy's field names. A field the user renamed
  (`withFieldsUpdated { case f => f.copy(name = ...) }`) or added (`withField`) is not in it, so no profile renames it.
- `Lowering` renames, in every `object` it walks, the fields in `scalaNamedFields` to `p.fieldName(name)`, and clears
  the set (the result is shaped; `withProfile(p1)` then a render under `p2` renames nothing again: the "shaped
  already" rule of `withProfile`). Under `Identity` the names don't change; the set is cleared all the same.
- The value-class member of a family, which a library profile writes as the case class `{"<field>": value}`
  (`Lowering.own`), gets `p.fieldName(field)`: the library derives that member's codec with the same config.
- Two fields of one object that end up with the same name (`fooBar` and `foo_bar` under snake_case) are an
  `IllegalArgumentException` naming both and the profile, as for other settings a profile can't describe.
- Not renamed: hand-written `Schema.object` fields (no fact), the parser module's objects, map keys, the
  discriminator field, the WrapperObject key, Legacy's `{"value": X}` Option wrapper (built by the lowering).

The bridges stop throwing and read the setting (the `renamed` sample check stays, to keep `Identity`, so the default
profiles still compare equal):

| profile | `fieldName` |
|---|---|
| `PlayProfile.fromConfig(cfg)` | `Custom(cfg.naming)` if it renames, else `Identity` |
| `CirceProfile.configured(..., transformMemberNames)`, Scala 3 `fromConfig(c)` | `Custom(transformMemberNames)` if it renames, else `Identity` |
| `UPickleProfile.fromApi(api)` | `Custom(n => api.objectAttributeKeyWriteMap(n).toString)` if it renames, else `Identity` |
| `Json4sProfile`, `SprayProfile` | `Identity` (no global naming setting; `withFieldName` for yours) |

The repro with the play profile then renders:

```json
{"type":"object","additionalProperties":false,
 "properties":{"order_id":{"type":"string"},"item_count":{"type":"integer"},"gift_note":{"type":["string","null"]},
   "line":{"type":"object","additionalProperties":false,
     "properties":{"unit_price":{"type":"number"},"sku_code":{"type":"string"}},"required":["unit_price","sku_code"]}},
 "required":["order_id","item_count","line"]}
```

and #292 (PureConfig) becomes, with no PureConfig dependency (`ConfigFieldMapping` is a `String => String`):

```scala
val kebab = ConfigFieldMapping(CamelCase, KebabCase)
implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Neutral
  .withSum(SumStyle.Discriminator("type")).withMemberName(MemberName.Custom(m => kebab(m.name)))
  .withSingleton(SingletonStyle.AsTagged).withFieldName(FieldName.Custom(kebab))
```

Pros: fixes every library with one rule; the derived tree stays library-neutral (derive once, render under any
profile); matches what the libraries rename and nothing else; implicit `Json.schema[X]` definitions (derived) are
renamed like inline ones, hand-written implicit Schemas are used as they are (README's "used as it is" rule); a
mixed setup (one type with another naming) is `implicit val s = Json.schema[X].withProfile(itsProfile)` for that
type. Purely additive API; default output unchanged.
Cons: a new fact on `object` that equality ignores, so two trees that compare equal can render differently under a
renaming profile (a derived object and a hand-written one with the same fields). Every derived object carries the
set (a small `Set[String]` per object in the generated code).

### B. The same knob, applied to every field of every object  — LOST

No fact in the tree: `Lowering` renames every field it meets. Smallest change (the knob, the lowering, the bridges).
It renames keys nobody renamed: a hand-written implicit `Schema[Money]` = `Schema.object(Field("amount_minor", ...))`
for a hand-written codec, objects the parser module read from a JSON Schema, a user's Predef. Snake_case and
kebab-case of an already converted name are usually no-ops, which hides the bug until a non-idempotent naming
(play `JsonNaming.PascalCase`: `amount_minor` → `Amount_minor`; upickle's upper-casing in our own test; any prefixing
function) rewrites the hand-written keys. It also contradicts "a type with an implicit `Schema` is used as it is".
Lost on correctness; it is the fallback if the owner doesn't want a new fact in the model.

### C. The fact on `Field` instead of on `object`  — LOST

`Field.scalaName: Option[String]`, renamed while `name == scalaName`. Per-field precise like A, and travels with the
field through `withFieldsUpdated`. But `Field` is a public case class: a new constructor parameter changes `apply`,
`copy` and `unapply` (binary breaking, 0.10.0 under PVP; 0.9.0 added `hasDefault` the same way at a minor bump).
In equality it breaks every tree spec that compares a derived tree with hand-written `Field(...)`s (~30 spec
files); outside equality it's A with a worse compatibility story. A gives the same behavior as a patch.

### D. Naming at derivation time (tapir-style implicit config)  — LOST

The macro looks for an implicit naming config where it derives (`implicit val n: json.schema.FieldNaming = ...`)
and writes the renamed names into the tree. That is how tapir does it (#270 suggests it), but here it would undo the
0.8/0.9 design: the derived tree is library-neutral and the profile at render time decides the wire shape. The
bridges couldn't read the library's config (the macro would need the user to restate it), Scala 3 `derives`
resolves it at the companion, far from the codec's config, and one tree couldn't be rendered for two libraries.
Lost.

### E. Keep the error, document the workaround  — LOST

Status quo plus a README recipe (`withFieldsUpdated`). The recipe renames one object, not nested ones, definitions or
family members; a full one is a user-written tree walk over every node kind. Lost: it is the gap the issues report.

## Decision

A: a `fieldName` knob (`FieldName.Identity` / `Custom(f)`), applied by the profile's lowering to the fields the
derivation took from case-class parameters, recorded as a non-rendered `scalaNamedFields` fact on `object`. The play,
circe and upickle profiles read their library's field naming instead of throwing. Default output is unchanged
everywhere; additive under PVP (0.9.x).

To choose another option instead: B is A without steps 1–3 below (the lowering renames every field); C moves the
fact to `Field` (step 1 changes, 0.10.0); E is "do nothing but docs".

## Implementation plan

1. **Model** (`core/src/main/scala/json/Schema.scala`, `` `object` ``): a `private var _scalaNamedFields: Set[String] = Set.empty`,
   `def scalaNamedFields`, `def withScalaNamedFields(names: Set[String]): `object`[T]` (a copy via `rebuild`, keeps
   the extras). `rebuild(fs)` and `strict` / `Free.of` carry the set intersected with `fs.map(_.name)`. Not in
   `equals`, `hashCode`, `toString` or `AsTree`. Scaladoc: the fact, who sets it, that profiles clear it.
2. **Scala 2 macro** (`macros/src/main/scala-2/.../SchemaTypes.scala`, `Obj.prefix`): for a case class (not a tuple:
   `definitions.isTupleType(tpe)`), append `.withScalaNamedFields(Set(<names>))` (a literal `Set` of the field names;
   nothing for zero fields). Check `Json.objectSchema` (Macroses.scala:65) goes through the same `Obj`.
3. **Scala 3 derivation** (`core/src/main/scala-3/.../derivation/Products.scala`, `Obj.emitRaw`): for
   `ProductKind.CaseClass`, emit `` Schema.`object`.fromList[t]($fs).withScalaNamedFields(Set(...)) ``. Scala 3
   `enum` cases with parameters go through `Obj` as case classes (check). `clean` before testing (macros).
4. **Profile** (`core/src/main/scala/json/schema/JsonSchemaProfile.scala`): `FieldName` (`Identity`, `Custom`), the
   `fieldName` member (default `Identity`), `withFieldName`, a `fieldName` field in `Knobs` (and `Knobs.apply(p)`).
   `Lowering.apply` for `object`: rename the fields in `o.scalaNamedFields` with `p.fieldName`, recurse into types
   (as now), then check duplicate names (throw `IllegalArgumentException(s"${p.name}: fieldName maps a and b to x in object(...)")`),
   then `withScalaNamedFields(Set.empty)`. `Lowering.own` (value-class member): `Field(p.fieldName(f), inner, ...)`.
   Update the `lower` scaladoc ("Idempotent ...": a schema without `option`/`family` nodes and without Scala-named
   fields comes back equal) and the trait scaladoc (what a profile lowers).
5. **Bridges**: `PlayProfile.fromConfig`, `CirceProfile.configured` (Scala 3 `fromConfig` delegates already),
   `UPickleProfile.fromApi`: drop the throw, set `withFieldName` as in the table above; update their scaladocs
   ("Throws ... renames fields" goes; upickle keeps its `optionsAsNulls` throw).
6. **Tests, core/api** (`api/src/test/scala/com/github/andyglow/jsonschema/ProfileSpec.scala`, shared, all versions;
   a Scala 3 case under `scala-3/` for `derives` and `enum`):
   - the fact: a derived case class has all its fields in `scalaNamedFields`, a tuple none, `Schema.object(...)` none;
     `withField` / user renames via `withFieldsUpdated` leave them out; `duplicate`, `toDefinition`, `free`/`strict`
     keep it; equality ignores it;
   - `Neutral.withFieldName(Custom(snake))`: top-level, nested, in an implicit `Json.schema` definition, in a
     recursive type, family members (case class, value class `{"<field>"}`, tagged under a `Discriminator`, wrapped
     under `WrapperObject`: the tag field and wrapper key unchanged), `required` and `default` follow the new names,
     map keys unchanged, hand-written implicit `Schema` unchanged, Legacy's `{"value"}` unchanged;
   - idempotence: `p.lower(p.lower(s)) == p.lower(s)`; `s.withProfile(Neutral)` rendered under a renaming profile
     keeps the Scala names; the collision error;
   - `Identity`: every golden file unchanged (no golden or golden-legacy edits expected; a diff is a bug).
7. **Tests, bridges** (acceptance, networknt): replace the "fail fast" cases with real-codec cases that validate:
   play `JsonConfiguration(JsonNaming.SnakeCase)` (plus `assertInvalid` under `AsPlay._` Neutral, the repro);
   circe Scala 2 generic-extras `Configuration.default.withSnakeCaseMemberNames` (`CirceExtrasSpec`) and
   `CirceProfile.configured(transformMemberNames = ...)` (`CirceAcceptanceSpec`); circe Scala 3
   `Configuration.default.withSnakeCaseMemberNames` + `derives ConfiguredCodec` (`Circe3Spec`); upickle `SnakeFields`
   (already in `UPickleAcceptanceSpec`, upper-cases keys). Use a model with a nested case class, an `Option` field and
   a family member. "read settings" tests: the default profiles' `fieldName` is `Identity`.
8. **JS**: nothing platform-specific (plain string functions; play-json, circe, upickle run on JS). The new
   acceptance cases cancel on JS like the others; the ProfileSpec cases run there. Run `rootJS/test`.
9. **Docs** (README and `docs/src/main/paradox/rendering.md`, same words): the knob table (`withFieldName`,
   `FieldName.Identity`, `Custom(scalaName => name)`, "the JSON name of a case-class field"); the library table's
   "read from" column (play `naming`, circe `transformMemberNames`, upickle `objectAttributeKeyWriteMap`); replace
   "Renamed fields ... are an error" with what is renamed and what isn't (hand-written schemas, `@JsonKey` / `@key`
   per-field annotations still unseen); a PureConfig recipe (the snippet above, plain `scala`: no PureConfig in the
   docs build) closing #292's remainder.

Risks: an `object` built by a user's own macro or code from a derived one by `new`/`Free.of` paths that skip
`rebuild` loses the fact (renamed nothing: today's behavior). Per-type namings (two `JsonConfiguration`s in one
model) need the `withProfile` per type; a knob that sees the owner type would need the owner in the tree (not in
scope).

## Not in scope

- The libraries' per-field annotations (circe-generic-extras `@JsonKey`, play `@JsonKey`, upickle `@key`): the
  derivation would have to read foreign annotations. Still listed in the README as unseen.
- A `PureConfigProfile` module (the recipe above covers it; a module is a new dependency to maintain).
- Built-in `snake_case` / `kebab-case` functions in core: the libraries disagree on edge cases (`URL`, `x1`), so the
  profile takes the library's own function.
- Tuples rendered as objects of `_1`, `_2` while libraries write arrays: a separate mismatch.

## Compatibility

- Source/binary: additive. A new trait member with a default (`fieldName`), new builder, new `FieldName` ADT, new
  methods on `` `object` ``. `Field`, `object`'s constructor and every existing signature unchanged. Code compiled
  against 0.9.0 that implements `JsonSchemaProfile` keeps working (the default method). Generated code calls the new
  `withScalaNamedFields`, so code compiled with the new macro needs the new core (as with any release).
- Output: unchanged for every profile and config that works today (`Identity` everywhere by default). Configs that
  threw (`PlayProfile.fromConfig` with a renaming `naming`, `CirceProfile.configured`/`fromConfig` with
  `transformMemberNames`, `UPickleProfile.fromApi` with a renaming `objectAttributeKeyWriteMap`) now return a profile
  that renders renamed keys. That removes an exception users may test for: called out in the PR and changelog.
- PVP: a 0.9.x patch-level addition (no breaking change). Legacy: unchanged (`Identity`), golden-legacy untouched.

## Review log
