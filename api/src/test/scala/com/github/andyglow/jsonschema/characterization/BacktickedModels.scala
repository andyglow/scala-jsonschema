package com.github.andyglow.jsonschema.characterization

// No doc comments in this file: on Scala 2 an undocumented class picks up the nearest preceding doc comment (scaladoc-parser leak, BACKLOG #5)

case class `odd name`(x: Int)
object `odd name` { implicit val oddSchema: json.Schema[`odd name`] = json.Json.schema[`odd name`] }
case class HasOdd(o: `odd name`)
