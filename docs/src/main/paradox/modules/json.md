# JSON libraries

The library renders schemas into its own JSON AST (`com.github.andyglow.json.Value`). A bridge module renders
into your JSON library's AST instead:

```scala mdoc:compile-only
import json.schema.Version._
import com.github.andyglow.jsonschema.AsPlay._
import play.api.libs.json._

case class Search(query: String, tags: List[String] = List("all"))

val schema: JsObject = json.Json.schema[Search].asPlay(Draft07("https://example.com/search.json"))
```

| module                         | import                                       | call                       | result                       |
|--------------------------------|----------------------------------------------|----------------------------|------------------------------|
| `scala-jsonschema-play-json`   | `com.github.andyglow.jsonschema.AsPlay._`    | `schema.asPlay(version)`   | `play.api.libs.json.JsObject` |
| `scala-jsonschema-circe-json`  | `com.github.andyglow.jsonschema.AsCirce._`   | `schema.asCirce(version)`  | `io.circe.Json`              |
| `scala-jsonschema-spray-json`  | `com.github.andyglow.jsonschema.AsSpray._`   | `schema.asSpray(version)`  | `spray.json.JsValue`         |
| `scala-jsonschema-json4s-json` | `com.github.andyglow.jsonschema.AsJson4s._`  | `schema.asJson4s(version)` | `org.json4s.JValue`          |
| `scala-jsonschema-ujson`       | `com.github.andyglow.jsonschema.AsU._`       | `schema.asU(version)`      | `ujson.Value`                |

These libraries have their own `Json` object, so write `json.Json.schema` when you import their package with `_`.

The bridges also provide a `ToValue` from the library's encoders (`Writes`, `Encoder`, ...). With it, default
values of your own types are rendered (`"default": ["all"]` above), and enum values come from your codec.
