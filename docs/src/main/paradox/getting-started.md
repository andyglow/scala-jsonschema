# Getting Started

## Install

```scala
libraryDependencies += "io.github.andyglow" %% "scala-jsonschema" % "0.8.0"
```

From 0.8.0 on, the groupId is **`io.github.andyglow`**. Releases up to 0.7.x are under `com.github.andyglow`.
The Scala packages (`json.*`, `com.github.andyglow.*`) haven't changed.

- Scala 2.12, 2.13 and 3.3 LTS (and later 3.x). Scala 2.11 support ended with 0.7.x.
- Runs on Java 8 or later.
- `scala-jsonschema` pulls in `scala-jsonschema-core` and `scala-jsonschema-macros`.

The optional modules (JSON-library bridges, refined, cats, enumeratum, joda-time, derived, parser) have the same
version; see @ref:[Modules](modules/index.md).

## Quick start

```scala mdoc:compile-only
import json._
import json.schema.Version._

case class Company(name: String)
case class Person(name: String, nickname: Option[String], age: Int = 18, company: Company)

val schema: json.Schema[Person] = Json.schema[Person]

println(Json.stringify(schema, Draft07("https://example.com/person.json")))
```

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://example.com/person.json",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "name": { "type": "string" },
    "nickname": { "type": "string" },
    "age": { "default": 18, "type": "integer" },
    "company": {
      "type": "object",
      "additionalProperties": false,
      "properties": { "name": { "type": "string" } },
      "required": ["name"]
    }
  },
  "required": ["name", "company"]
}
```

`Option` fields and fields with a default value are optional. A default is written into the schema when it can be
turned into JSON: primitives, strings and collections/maps of them always can, other types need a `ToValue`, which
the JSON-library bridges derive from your encoders.

The same code works on Scala 3. There you can also derive the schema with the type, which makes each such type a
@ref:[definition](definitions.md):

```scala
case class Company(name: String) derives json.Schema
case class Person(name: String, nickname: Option[String], age: Int = 18, company: Company) derives json.Schema

val schema: json.Schema[Person] = Json.schema[Person] // or summon[json.Schema[Person]]
```

## Entry points

| call                                   | result                                                         |
|----------------------------------------|----------------------------------------------------------------|
| `Json.schema[T]`                       | `json.Schema[T]`, derived at compile time                      |
| `Json.objectSchema[T]("field" -> "description", ...)` | the schema of a case class as `json.Schema.object[T]`, with field descriptions |
| `Json.sig[T].signature`                | the type's name as used for definitions                        |
| `Json.stringify(schema, version)`      | the rendered JSON as a `String`                                |
| `import Json.auto._` (Scala 3: also `import Json.auto.given`) | an implicit `json.schema.Predef[T]` for every derivable `T` |
