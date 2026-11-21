# Parser

`scala-jsonschema-parser` goes the other way, with no dependencies: `ParseJson` parses JSON into the library's
AST, and `ParseJsonSchema` parses a JSON Schema (a draft-04 subset) into a `json.Schema`.

```scala mdoc:compile-only
import com.github.andyglow.json.ParseJson
import com.github.andyglow.jsonschema.ParseJsonSchema
import scala.util.Try

val value: Try[com.github.andyglow.json.Value] = ParseJson("""{"name": "x"}""")

val schema: Try[json.Schema[_]] = ParseJsonSchema("""{
  "type": "object",
  "properties": { "name": { "type": "string" } },
  "required": ["name"]
}""")
```
