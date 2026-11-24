# Parser

`scala-jsonschema-parser` goes the other way, with no dependencies: `ParseJson` parses JSON into the library's
AST, and `ParseJsonSchema` parses a JSON Schema (a draft-04 subset, including `"type": [X, "null"]`) into a
`json.Schema`. `{}` (or an object of only annotations such as `title`) is `` Schema.`any` ``. The boolean schemas
`true` and `false` (draft-06+, read in a draft-04 document too) are `any` and `not(any)` in `items` and `properties`.

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
