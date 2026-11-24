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

## Any JSON value

`` json.Schema.`any`[T]() `` accepts any JSON value: it renders `{}` in every draft. Use it for a field whose content
you don't describe:

```scala
import com.typesafe.config.ConfigValue

implicit val configValuePredef: json.schema.Predef[ConfigValue] = json.schema.Predef(json.Schema.`any`[ConfigValue]())
```

The JSON libraries' own AST types get theirs from an import, one per bridge. Our own
`com.github.andyglow.json.Value` needs none.

| AST type                   | `{}` (any)    | free object   | array of any | string, number, boolean            | import                                              |
|----------------------------|---------------|---------------|--------------|------------------------------------|-----------------------------------------------------|
| ours                       | `Value`       | `Value.obj`   | `Value.arr`  | `Value.str`, `Value.num`, `Value.bool` | none                                            |
| play-json                  | `JsValue`     | `JsObject`    | `JsArray`    | `JsString`, `JsNumber`, `JsBoolean` | `com.github.andyglow.jsonschema.AsPlay.predef._`   |
| circe                      | `Json`        | `JsonObject`  |              | `JsonNumber`                       | `com.github.andyglow.jsonschema.AsCirce.predef._`  |
| ujson                      | `ujson.Value` | `ujson.Obj`   | `ujson.Arr`  | `ujson.Str`, `ujson.Num`, `ujson.Bool` | `com.github.andyglow.jsonschema.AsU.predef._`  |
| json4s                     | `JValue`      | `JObject`     | `JArray`, `JSet` | `JString`, `JDouble`, `JDecimal`, `JLong`, `JInt`, `JNumber`, `JBool` | `com.github.andyglow.jsonschema.AsJson4s.predef._` |
| spray-json                 | `JsValue`     | `JsObject`    | `JsArray`    | `JsString`, `JsNumber`, `JsBoolean` | `com.github.andyglow.jsonschema.AsSpray.predef._`  |

```scala mdoc:compile-only
import play.api.libs.json.{JsObject, JsValue}
import com.github.andyglow.jsonschema.AsPlay.predef._

case class Event(id: String, payload: JsValue, metadata: JsObject)

json.Json.schema[Event] // "payload": {}, "metadata": {"type": "object", "additionalProperties": true}
```

- The import is separate from `AsPlay._` (and the other bridges' `AsX._`), so a `Predef` of your own for one of these
  types doesn't become ambiguous. They are `Predef`s, so they are inlined. For a definition, declare
  `` implicit val s: json.Schema[JsValue] = json.Schema.`any`[JsValue]() ``.
- `Option[JsValue]` renders `{}` (it accepts `null` already), not `oneOf[{}, {"type": "null"}]`, which would reject `null`.
- A default value (`payload: JsValue = Json.obj()`) needs the bridge's `ToValue`: import `AsPlay._` (or
  `profiles.play._`) too, as for any other default.
- Don't put an `any` into a `oneOf`: it overlaps every other member, so the `oneOf` rejects their values.
- json4s leaves out a field whose value is `JNothing`, so a required `JValue` field then fails `required`. Make such a
  field an `Option[JValue]` or give it a default.
- Without the import, json4s' and spray's ASTs derive as families of their Scala classes (a JSON string would have to
  be `{"s": "..."}`), and the other libraries' don't compile.
