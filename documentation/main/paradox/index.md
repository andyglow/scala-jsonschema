# Scala ↦ JsonSchema

@@@ index

* [Getting Started](getting-started.md)
* [Design](design.md)
* [Types](types.md)
* [Type Constraints](type-constraints.md)
* [Document your model](document-it.md)
* [Customization](customization.md)

@@@

## Overview

Derives [JsonSchema](https://json-schema.org/) directly out of your types. 

## Features
- Versions 
    - <input type="checkbox" checked="true" disabled="true"> `draft-04`
    - <input type="checkbox" checked="true" disabled="true"> `draft-06`
    - <input type="checkbox" checked="true" disabled="true"> `draft-07`
- <input type="checkbox" checked="true" disabled="true"> scala native classes (`scala.Option`, e.g.)
- <input type="checkbox" checked="true" disabled="true"> scala collection classes
- <input type="checkbox" checked="true" disabled="true"> `case classes`
- <input type="checkbox" checked="true" disabled="true"> `value classes`
- <input type="checkbox" checked="true" disabled="true"> `sealed trait enums`
- <input type="checkbox" checked="true" disabled="true"> `sealed trait case classes`
- <input type="checkbox" checked="true" disabled="true"> `generic types`
- <input type="checkbox" checked="true" disabled="true"> `recursive types`
- <input type="checkbox" checked="true" disabled="true"> `default values`
- Extensions
    - <input type="checkbox" checked="true" disabled="true"> [joda-time](https://github.com/JodaOrg/joda-time)
    - <input type="checkbox" checked="true" disabled="true"> [cats](https://github.com/typelevel/cats)
    - <input type="checkbox" checked="true" disabled="true"> [refined](https://github.com/fthomas/refined)
    - <input type="checkbox" checked="true" disabled="true"> [enumeratum](https://github.com/lloydmeta/enumeratum)
- Integrations
    - <input type="checkbox" checked="true" disabled="true"> [play-json](https://github.com/playframework/play-json)
    - <input type="checkbox" checked="true" disabled="true"> [spray-json](https://github.com/spray/spray-json)
    - <input type="checkbox" checked="true" disabled="true"> [json4s](https://github.com/json4s/json4s)
    - <input type="checkbox" checked="true" disabled="true"> [circe](https://github.com/circe/circe)
    - <input type="checkbox" checked="true" disabled="true"> [ujson](https://www.lihaoyi.com/post/uJsonfastflexibleandintuitiveJSONforScala.html)
    
## Basic Example

Let's take a look at the example code 
```scala mdoc
sealed trait Event
object Event {
    final case class NewUser(firstName: String, lastName: String, login: String, password: String) extends Event
    final case class UpdateUserProfile(id: String, firstName: Option[String], lastName: Option[String]) extends Event
}
case class EventEnvelope(
    id: String,
    headers: Map[String, String],
    body: Event)
```    

Derive Schema:
```scala mdoc:silent
import json._

val eventEnvelopeSchema: json.Schema[EventEnvelope] = Json.schema[EventEnvelope]
```

Result:
```scala mdoc:height=200
import com.github.andyglow.jsonschema._

eventEnvelopeSchema.draft04
```