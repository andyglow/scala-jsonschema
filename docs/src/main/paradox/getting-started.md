# Getting Started

## Install

```scala
libraryDependencies += "io.github.andyglow" %% "scala-jsonschema" % "0.9.0"
```

From 0.8.0 on, the groupId is **`io.github.andyglow`**. Releases up to 0.7.x are under `com.github.andyglow`.
The Scala packages (`json.*`, `com.github.andyglow.*`) haven't changed.

- Scala 2.12, 2.13 and 3.3 LTS (and later 3.x). Scala 2.11 support ended with 0.7.x.
- Runs on Java 8 or later.
- `scala-jsonschema` pulls in `scala-jsonschema-core` and `scala-jsonschema-macros`.

The optional modules (JSON-library bridges, refined, cats, enumeratum, joda-time, derived, parser) have the same
version; see @ref:[Modules](modules/index.md).

## Scala.js

From 0.9.0 on, `scala-jsonschema` (with `-core` and `-macros`) and most modules are also published for Scala.js 1.x,
for Scala 2.12, 2.13 and 3. Use `%%%`:

```scala
libraryDependencies += "io.github.andyglow" %%% "scala-jsonschema" % "0.9.0"
```

- Needs **Scala.js 1.23.0** or later (`sbt-scalajs` 1.23.0): a project's linker must be at least as new as the
  Scala.js IR in the jars it links.
- spray-json, json4s and joda-time have no Scala.js builds, so their modules are JVM only.
- Schemas are derived at compile time, so everything works as on the JVM: scaladoc descriptions, `derives json.Schema`,
  refined types, defaults. On Scala 3, scaladoc of classes from another module needs `-Yread-docs`, as on the JVM.
- `java.time` types (`LocalDate`, `Instant`, ...) need no extra dependency: their schema only names the type. Values of
  those types at run time need a `java.time` implementation such as
  [scala-java-time](https://github.com/cquiroz/scala-java-time), as in any Scala.js code. So does the schema of a
  Java enum from `java.time` (`DayOfWeek`, `Month`): a Java enum's schema lists its values at run time.
  `java.util.UUID` and `java.net.URI` are part of Scala.js; `java.sql` types don't exist there.
  The schemas (and models with fields) of `Locale`, `Currency`, `InetAddress`, `Path` and `File` compile and link on
  Scala.js; creating values of `Locale`/`Currency` needs [scala-java-locales](https://github.com/cquiroz/scala-java-locales),
  the others have no Scala.js implementation.
- Numbers render as on the JVM: a `Double` bound of `16.0` is `16.0` in the JSON, not `16`.

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
    "nickname": { "type": ["string", "null"] },
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

`Option` fields and fields with a default value are optional. An `Option[X]` is "X or null" wherever it appears
(see @ref:[Option](rendering.md#option)). A default is written into the schema when it can be turned into JSON:
primitives, strings and collections/maps of them always can, other types need a `ToValue`, which the JSON-library
bridges derive from your encoders.

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
