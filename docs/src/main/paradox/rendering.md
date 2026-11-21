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

<!-- profiles09: per-library sections are filled in after the bridges09 merge -->

What a profile describes (the same rules for every library):

- what the library writes, widened only by what its reader also accepts (play reads a missing `Option` field and
  `null`, so both validate); value classes are unwrapped, as everywhere;
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
