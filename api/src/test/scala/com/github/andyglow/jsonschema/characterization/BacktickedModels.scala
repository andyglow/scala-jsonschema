package com.github.andyglow.jsonschema.characterization

case class `odd name`(x: Int)
object `odd name` { implicit val oddSchema: json.Schema[`odd name`] = json.Json.schema[`odd name`] }
case class HasOdd(o: `odd name`)
