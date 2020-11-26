# Type Constraints

Type Constraints in the world of `jsonschema` called `Validations`. 
It is also possible to add specific validation rules to our schemas.

Available validations:

- multipleOf
- maximum
- minimum
- exclusiveMaximum
- exclusiveMinimum
- maxLength
- minLength
- pattern
- maxItems
- minItems
- uniqueItems
- maxProperties
- minProperties

Example
```scala
import json._
import json.Validation._

implicit val vb = ValidationBound.mk[UserId, String]

implicit val userIdSchema: json.Schema[UserId] = Json.schema[UserId]("userId") withValidation (
  `pattern` := "[a-f\\d]{16}"
)
``` 
Definition will look then like
```json
{
  "userId": {
    "type": "string",
    "pattern": "[a-f\\d]{16}"
  }
}
```