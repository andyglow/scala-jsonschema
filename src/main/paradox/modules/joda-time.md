# Joda Time

Joda Time Support allows you to use joda-time classes within your models.
Here is an example.
 
```scala
import com.github.andyglow.jsonschema.JodaTimeSupport._
import org.joda.time._

case class Event(id: String, timestamp: Instant)

val eventSchema: Schema[Event] = Json.schema[Event]

println(JsonFormatter.format(AsValue.schema(eventSchema)))
```
results in
```
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "id": {
      "type": "string"
    },
    "timestamp": {
      "$ref": "#/definitions/org.joda.time.Instant"
    }
  },
  "required": [
    "id",
    "timestamp"
  ],
  "definitions": {
    "org.joda.time.Instant": {
      "type": "string",
      "format": "date-time"
    }
  }
}
```