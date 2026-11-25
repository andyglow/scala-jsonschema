# Rendering

A `json.Schema` is independent of the JSON Schema draft and of any JSON library. You pick the draft when rendering:

| version (`json.schema.Version`) | `$schema`                                       | definitions             | `$ref`                        |
|---------------------------------|-------------------------------------------------|-------------------------|-------------------------------|
| `Draft04()`                     | `http://json-schema.org/draft-04/schema#`       | `definitions`           | `#/definitions/<name>`        |
| `Draft06(id)`, `Draft07(id)`    | `http://json-schema.org/draft-0{6,7}/schema#`   | `definitions` + `$id`   | `#<name>`                     |
| `Draft09(id)` (2019-09)         | `https://json-schema.org/draft/2019-09/schema`  | `$defs` + `$anchor`     | `#<name>`                     |
| `Draft12(id)` (2020-12)         | `https://json-schema.org/draft/2020-12/schema`  | `$defs` + `$anchor`     | `#<name>`                     |

```scala mdoc:compile-only
import json._
import json.schema.Version._
import com.github.andyglow.jsonschema.AsValue

case class Person(name: String)

val schema = Json.schema[Person]

Json.stringify(schema, Draft12("https://example.com/person.json")) // String
AsValue.schema(schema, Draft04())                                    // com.github.andyglow.json.Value, the library's own AST
```

To render into a JSON library's AST, see @ref:[JSON libraries](modules/json.md).

## Option

Every `Option[X]` is "X or null": at the root, as a collection element, as a map value and as a field. An `Option`
field is also not required, and `Option[Option[X]]` is the same as `Option[X]`. This is what circe, play-json,
spray-json and upickle write (`None` is `null`, or the field is left out) and read back (upickle's reader wants
the key present unless the field has a default).

```scala
case class Pet(name: String, age: Option[Int], tags: List[Option[String]], owner: Option[Owner])
```

Draft-04, abridged (`Owner` has an implicit `Schema`, so it is a definition):

```json
{
  "type": "object",
  "properties": {
    "name": { "type": "string" },
    "age": { "type": ["integer", "null"] },
    "tags": { "type": "array", "items": { "type": ["string", "null"] } },
    "owner": { "oneOf": [{ "$ref": "#/definitions/Owner" }, { "type": "null" }] }
  },
  "required": ["name", "tags"]
}
```

Where X has one JSON type (a string, number, integer, boolean, array or object), the output is X with
`"type": [X, "null"]`. Otherwise (an enum, a `$ref`, a sealed family) it is `oneOf: [X, {"type": "null"}]`.

The macro keeps the fact "this is an `Option`" in the schema as an `` `option` `` node, and the renderer shapes it
with a `json.schema.JsonSchemaProfile`. The default profile is `JsonSchemaProfile.Neutral`, as above.
`JsonSchemaProfile.Legacy` renders exactly what 0.7 rendered: an `Option` field is X and not required, any other
`Option[X]` is `oneOf: [{"type": "string", "enum": ["None"]}, {"type": "object", "properties": {"value": X}, ...}]`.
Use it for one schema, or for every render where an implicit profile is in scope:

```scala mdoc:compile-only
import json._
import json.schema.JsonSchemaProfile
import json.schema.Version._

case class Pet(name: String, age: Option[Int])

val legacySchema = Json.schema[Pet].withProfile(JsonSchemaProfile.Legacy) // a json.Schema with the 0.7 shapes

implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy        // Scala 3: given JsonSchemaProfile = ...
Json.stringify(Json.schema[Pet], Draft07("id"))                           // also AsValue.schema, asPlay, asCirce, ...
```

`withProfile` shapes the schema right away, so a later render keeps that shape whatever profile is in scope. To
share one profile across a project, define it in an object and import it where you render. In Scala 3, `import
Settings.*` doesn't import a `given`: write `import Settings.given`, or declare the profile as an `implicit val`.

## Library profiles

JSON libraries write sealed families differently. For

```scala
sealed trait Animal
final case class Dog(name: String) extends Animal
case object Fish                   extends Animal
```

play-json writes `{"_type": "petshop.Dog", "name": "Rex"}` and `{"_type": "petshop.Fish"}`, circe-generic
`{"Dog": {"name": "Rex"}}` and `{"Fish": {}}`, upickle `{"$type": "Dog", "name": "Rex"}` and `"Fish"`, json4s
`{"name": "Rex"}` and `{}`. The default schema (an untagged `oneOf`, case objects as a string enum, as in 0.7)
matches none of them. So the macro records the facts of a sealed family (its members, their names and kinds, the
annotations) in the schema as a `` `family` `` node, like `Option`, and a profile shapes it when you render.

One import gives rendering, `ToValue` and the library's profile (`AsPlay._`, `AsCirce._`, ... stay as they are
and bring no profile):

```scala
import com.github.andyglow.jsonschema.profiles.play._    // asPlay: what Json.format writes under the implicit JsonConfiguration in scope
import com.github.andyglow.jsonschema.profiles.circe._   // asCirce: circe-generic (semiauto); Scala 3: a given io.circe.derivation.Configuration in scope wins
import com.github.andyglow.jsonschema.profiles.upickle._ // asU: upickle.default
import com.github.andyglow.jsonschema.profiles.json4s._  // asJson4s: the implicit Formats in scope, else DefaultFormats
import com.github.andyglow.jsonschema.profiles.spray._   // asSpray: spray derives no sum codecs, so Neutral's shapes (state yours, below)
```

In Scala 3 write `.*`: the bridges declare `implicit def`s, which `.*` imports.

| library | as it writes `Dog` / `Fish` | profile | read from |
|---|---|---|---|
| play-json | `{"_type": "petshop.Dog", "name": ...}` / `{"_type": "petshop.Fish"}` | `PlayProfile.fromConfig(cfg)`, `PlayProfile.default` | `discriminator`, `typeNaming` (given the full name); `optionHandlers` needs nothing (play reads both) |
| circe | `{"Dog": {...}}` / `{"Fish": {}}`; configured: `{"type": "Dog", ...}` | `CirceProfile.default`, `CirceProfile.configured(discriminator, transformConstructorNames, transformMemberNames)`, Scala 3: `CirceProfile.fromConfig(configuration)` | `discriminator`, `transformConstructorNames` |
| upickle | `{"$type": "Dog", "name": ...}` / `"Fish"` | `UPickleProfile.fromApi(api)`, `UPickleProfile.default` | `tagName`, `objectTypeKeyWriteFullyQualified`, `objectTypeKeyWriteMap`; an `Option` key must be present unless the field has a default |
| json4s | `{"name": ...}` / `{}`; with type hints `{"jsonClass": "Dog", ...}` | `Json4sProfile.fromFormats(formats)`, `Json4sProfile.default` | `typeHints`, per family: a family of hinted classes is tagged, one without hints isn't |
| spray-json | your `RootJsonFormat` | `SprayProfile.default` (Neutral) | nothing: say what your format writes |

Where the library takes its configuration implicitly, the profile follows the same implicit, so a custom
configuration changes the schema with no extra code:

```scala
// play's Json.format can't run inside mdoc's wrapper class, so this is plain code; the acceptance suite runs it
import play.api.libs.json._
import com.github.andyglow.jsonschema.profiles.play._
import json.schema.Version._

implicit val cfg: JsonConfiguration = JsonConfiguration(
  discriminator  = "_kind",
  typeNaming     = JsonNaming(_.split('.').last),           // "petshop.Dog" -> "Dog"
  optionHandlers = OptionHandlers.WritesNull
)
implicit val fishF: OFormat[Fish.type] = Json.format[Fish.type] // play's macros read `cfg`
implicit val dogF: OFormat[Dog]        = Json.format[Dog]
implicit val animalF: OFormat[Animal]  = Json.format[Animal]
implicit val petF: OFormat[Pet]        = Json.format[Pet]

json.Json.schema[Pet].asPlay(Draft12("pet"))                    // the same `cfg`: "_kind", "Dog", "age": ["integer", "null"]
```

The same holds for json4s (`implicit val formats: Formats = DefaultFormats + ShortTypeHints(List(classOf[Dog],
Fish.getClass), "kind")`) and, in Scala 3, circe's `given Configuration = Configuration.default.withDiscriminator("type")`
next to `derives ConfiguredCodec`. Where the configuration is not an implicit, pass it:

```scala
// upickle: your API object
object MyPickler extends upickle.AttributeTagged { override def tagName = "kind" }
implicit val profile: JsonSchemaProfile = UPickleProfile.fromApi(MyPickler)

// circe-generic-extras (Scala 2): the fields of its Configuration that shape the output
implicit val profile: JsonSchemaProfile =
  CirceProfile.configured(config.discriminator, config.transformConstructorNames, config.transformMemberNames)

// spray: what your hand-written RootJsonFormat[Animal] writes
implicit val profile: JsonSchemaProfile =
  SprayProfile.default.withSum(SumStyle.Discriminator("type")).withMemberName(MemberName.SimpleName).withSingleton(SingletonStyle.AsTagged)
```

With a profile of your own, import `AsX._` (rendering, no profile), not `profiles.<lib>._`. With both in scope,
Scala 2 reports the ambiguity with a message saying so; Scala 3 picks yours, the closer one. Two `profiles.<lib>._`
imports in one scope are an error too. Per call, `schema.withProfile(PlayProfile.fromConfig(cfg))` shapes one
schema whatever is in scope where it is rendered.

What the profiles can't see: the libraries' own annotations (upickle `@key`, circe-generic-extras `@JsonKey`, play's
`@JsonKey`) and hand-written codecs (give such a type an implicit `Schema`). Renamed fields (play `naming`, circe
`transformMemberNames`, upickle `objectAttributeKeyWriteMap`) are an error (tried on sample names). json4s 4 fails
to encode case classes on Scala 3 itself, so its profile is for Scala 2 users. A `Map` whose key play-json has no
`KeyWrites` for (e.g. `Map[UUID, V]`) is written by play as an array of pairs, which `PlayProfile` doesn't describe
(see @ref:[Supported types](types.md#supported-types)).

What a profile describes (the same rules for every library):

- what the library writes, widened only by what its reader also accepts (play reads a missing `Option` field and
  `null`, so both validate). Value classes are unwrapped (as fields, collection elements, ...); a value class that
  is a member of a sealed family is the case class `{"<field>": value}`, as the libraries' derived sum codecs
  write it;
- a member's tag (`_type`, `$type`, ...) and name come from the library's configuration, not from
  `@discriminator`/`@discriminatorKey`: the library's derivation never sees those. They apply under Neutral and
  Legacy, and under the spray profile, where you write the sum codec yourself. `@title`, `@description` and
  `@definition` apply under every profile;
- a setting the profile can't describe is an error when the profile is built (or the schema is lowered), naming
  the setting, not a schema that rejects the library's own output;
- a type with an implicit `Schema` or `Predef` is used as it is. A family member with an implicit `Schema` of an
  object gets the tag in a copy of it; one whose schema isn't an object can't carry a tag, and that is an error;
- nested sealed traits are flattened, as the libraries do; `Either` is not a family (still the 0.7 `oneOf`).

A profile of your own starts from one of the library profiles, or from `JsonSchemaProfile.Neutral`, and changes
knobs; each `with...` keeps the others:

| knob | values | |
|---|---|---|
| `withSum` | `SumStyle.Untagged`, `WrapperObject`, `Discriminator(field)`, `PerFamily(family => style)` | how members are told apart |
| `withSingleton` | `SingletonStyle.AsString`, `AsTagged` | a case object as a string, or as an object tagged like the other members |
| `withMemberName` | `MemberName.SimpleName`, `FullName`, `Custom(member => name)` | the tag of a member |
| `withOptionKeyRequired` | `true`, `false` | an `Option` field without a default is required (upickle) |
| `withHonorsSchemaAnnotations` | `true`, `false` | whether `@discriminator`/`@discriminatorKey` apply |
| `withOption` | `OptionStyle.Nullable`, `Legacy` | `Option` as "X or null", or the 0.7 shapes |
| `withOverlappingMembers` | `OverlappingMembers.AnyOf`, `OneOf` | an untagged family where one document could match two members: `anyOf`, or `oneOf` anyway (it then rejects that document) |

```scala mdoc:compile-only
import json._
import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile.{MemberName, SingletonStyle, SumStyle}
import json.schema.Version._

sealed trait Animal
case class Dog(name: String) extends Animal
case object Fish extends Animal

// a hand-written codec that writes {"type": "Dog", "name": ...} and {"type": "Fish"}
implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Neutral
  .withSum(SumStyle.Discriminator("type"))
  .withMemberName(MemberName.SimpleName)
  .withSingleton(SingletonStyle.AsTagged)

Json.stringify(Json.schema[Animal], Draft12("animal"))
```
