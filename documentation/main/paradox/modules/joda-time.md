# Joda Time

Joda Time Support allows you to use joda-time classes within your models.
Here is an example.
 
```scala mdoc
import json._

import com.github.andyglow.jsonschema._
import com.github.andyglow.jsonschema.JodaTimeSupport.predef._

import org.joda.time._

Json.schema[DateTime].stringify

case class Event(id: String, timestamp: Instant)

Json.schema[Event].stringify
```

If it makes sense to keep a joda-type schema a `definition` please use `import com.github.andyglow.jsonschema.JodaTimeSupport._` instead of `import com.github.andyglow.jsonschema.JodaTimeSupport.predef._`

```scala mdoc:reset
import json._

import com.github.andyglow.jsonschema._
import com.github.andyglow.jsonschema.JodaTimeSupport._

import org.joda.time._

Json.schema[DateTime].stringify

case class Event(id: String, timestamp: Instant)

Json.schema[Event].stringify
``` 