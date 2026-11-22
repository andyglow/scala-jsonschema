# Validation

`withValidation` adds validation keywords. Each one is checked against the schema's type at compile time (a
`maxLength` on an `Int` doesn't compile).

```scala mdoc:compile-only
import json._
import json.schema.validation.Instance._

Json.schema[String].withValidation(`minLength` := 1, `maxLength` := 100)
Json.schema[Int].withValidation(`minimum` := 0, `maximum` := 10)
Json.schema[Set[String]].withValidation(`maxItems` := 5)
```

| applies to         | keywords                                                                              |
|--------------------|---------------------------------------------------------------------------------------|
| numbers            | `multipleOf`, `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`            |
| strings            | `minLength`, `maxLength`, `pattern`, `contentEncoding`, `contentMediaType`, `contentSchema` |
| arrays             | `minItems`, `maxItems`, `uniqueItems`, `minContains`, `maxContains`                   |
| maps               | `minProperties`, `maxProperties`, `patternProperties`                                 |

For your own types (e.g. a value class), declare which group applies with a `json.schema.validation.Magnet`, as in
the `UserId` example in @ref:[Definitions](definitions.md#definition-names).

## Free objects

A free object allows any properties (`"additionalProperties": true`). Use it for a field that holds arbitrary JSON:

```scala mdoc:compile-only
import play.api.libs.json.JsObject

case class Payload(id: String, metadata: JsObject)

implicit val metadataSchema: json.Schema[JsObject] = json.Schema.`object`.Free[JsObject]()
// or, to inline it instead of making it a definition:
// implicit val metadataPredef: json.schema.Predef[JsObject] = json.schema.Predef(json.Schema.`object`.Free[JsObject]())

val payloadSchema = json.Json.schema[Payload]
```

Any object schema can be made free, and back:

```scala mdoc:compile-only
import json._

case class Person(name: String, age: Int)

val personSchema       = Json.objectSchema[Person]()
val freePersonSchema   = personSchema.free // additionalProperties: true
val strictPersonSchema = freePersonSchema.strict

strictPersonSchema == personSchema // true
```
