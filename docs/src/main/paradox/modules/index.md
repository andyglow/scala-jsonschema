# Modules

All modules have the groupId `io.github.andyglow` and the same version as `scala-jsonschema`.

| artifact                       | what it adds                                     | built against                         | Scala.js |
|--------------------------------|--------------------------------------------------|---------------------------------------|----------|
| `scala-jsonschema-play-json`   | `schema.asPlay(...)`, defaults via `Writes`      | `org.playframework:play-json` 3.0.x   | ✓        |
| `scala-jsonschema-circe-json`  | `schema.asCirce(...)`, defaults via `Encoder`    | `io.circe:circe-core` 0.14.x          | ✓        |
| `scala-jsonschema-spray-json`  | `schema.asSpray(...)`, defaults via `JsonWriter` | `io.spray:spray-json` 1.3.x           | JVM only |
| `scala-jsonschema-json4s-json` | `schema.asJson4s(...)`                           | `org.json4s:json4s-core` 4.0.x        | JVM only |
| `scala-jsonschema-ujson`       | `schema.asU(...)`                                | `com.lihaoyi:ujson`/`upickle` 4.x     | ✓        |
| `scala-jsonschema-refined`     | refined types as validations                     | `eu.timepit:refined` 0.11.x           | ✓        |
| `scala-jsonschema-cats`        | `NonEmptyList`, `NonEmptyMap`, ...               | `org.typelevel:cats-core` 2.x         | ✓        |
| `scala-jsonschema-enumeratum`  | enumeratum enums                                 | `com.beachape:enumeratum` 1.9.x       | ✓        |
| `scala-jsonschema-joda-time`   | joda-time types                                  | `joda-time:joda-time` 2.x             | JVM only |
| `scala-jsonschema-derived`     | the `DerivedSchema[T]` type class                |                                       | ✓        |
| `scala-jsonschema-parser`      | JSON and JSON Schema (draft-04 subset) to `json.Schema` | no dependencies                | ✓        |

On Scala.js (from 0.9.0), use `%%%`; see @ref:[Getting Started](../getting-started.md#scala-js).

Play JSON 3 is published as `org.playframework`. Projects still on `com.typesafe.play:play-json` 2.x should stay
on 0.7.x.

@@@ index

* [JSON libraries](json.md)
* [Refined](refined.md)
* [Cats](cats.md)
* [Enumeratum](enumeratum.md)
* [Joda-Time](joda-time.md)
* [DerivedSchema](derived.md)
* [Parser](parser.md)

@@@
