# Joda-Time

```scala mdoc:compile-only
import json._
import com.github.andyglow.jsonschema.JodaTimeSupport._
import org.joda.time._

case class Event(id: String, timestamp: Instant)

Json.schema[Event]
```

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "id": { "type": "string" },
    "timestamp": { "$ref": "#/definitions/org.joda.time.Instant" }
  },
  "required": ["id", "timestamp"],
  "definitions": {
    "org.joda.time.Instant": { "type": "string", "format": "date-time" }
  }
}
```

Supported: `Instant`, `DateTime`, `LocalDateTime` (format `date-time`), `LocalDate` (`date`), `LocalTime` (`time`)
and `Duration` (`duration`). They are implicit `Schema`s, so they appear as definitions. To inline them instead,
import the `Predef`s: `import com.github.andyglow.jsonschema.JodaTimeSupport.predef._`.
