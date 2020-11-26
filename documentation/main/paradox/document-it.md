# Document your Model

## Documentation
3 ways to maintain documented models are supported.

1. Annotations   
2. Config
3. Scaladoc

### Annotations
Scala-JsonSchema specifies 2 annotations that can help you specify a model 
`@title` and `@description` as well as fields `@description`s.

Example:
```scala
import json._
import json.schema._

@title("A Title")
@description("My perfect class")
case class Model(
    @description("A Param") a: String,
    @description("B Param") b: Int)

val schema = Json.objectSchema[Model]()
```
this, being translated into json, gets you
```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "description": "My perfect class",
  "title": "A Title",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "a": {
      "type": "string",
      "description": "A Param"
    },
    "b": {
      "type": "integer",
      "description": "B Param"
    }
  },
  "required": [
    "a",
    "b"
  ]
}
```

### Config
Another approach that you can use to keep your models concise, 
but documented is to provide documentation separately. As config.

Here is an example:
```scala
import json._

case class Model(a: String, b: Int)

val schema = Json.objectSchema[Model](
  "a" -> "A Param",
  "b" -> "B Param"
) .withDescription("My perfect class")
  .withTitle("A Title")
```
this, being translated into json, gets you the same effect as annotation based approach
```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "description": "My perfect class",
  "title": "A Title",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "a": {
      "type": "string",
      "description": "A Param"
    },
    "b": {
      "type": "integer",
      "description": "B Param"
    }
  },
  "required": [
    "a",
    "b"
  ]
}
```

This approach also nicely fits when models are specified in separate module or external library.

### Scaladoc
Also it is possible to infer descriptions from scaladoc. 
This allows to reuse scaladoc that you might want to have anyways.
This approach has it's own drawbacks, though.
- model classes must reside in the same module with schemas
- it requires non-incremental build or full-rebuild to take effect 

Example:
```scala
import json._

/** My perfect class
 * 
 * @param a A Param
 * @param b B Param
 */
case class Model(a: String, b: Int)
val schema = Json.objectSchema[Model]()
```
this, being translated into json, gets you
```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "description": "My perfect class",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "a": {
      "type": "string",
      "description": "A Param"
    },
    "b": {
      "type": "integer",
      "description": "B Param"
    }
  },
  "required": [
    "a",
    "b"
  ]
}
```
One little difference comparing to previous approaches is that this way you can't have `title` specified. 

### Combined approach
All these 3 techniques can be used all together.
The only thing you need to have in mind if going this way is that to extract different type 
of label Scala-JsonSchema will check certain sources in certain order.
 
| Element                         | Order                                   |
| ------------------------------- | --------------------------------------- |
| case class `title`              | `Config` -> `Annotation` -> `Scaladoc`  |
| case class `description`        | `Config` -> `Annotation` -> `Scaladoc`  |
| case class field `description`  | `Config` -> `Annotation` -> `Scaladoc`  |