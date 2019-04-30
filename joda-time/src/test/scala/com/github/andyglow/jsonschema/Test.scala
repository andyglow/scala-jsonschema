package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import com.github.andyglow.jsonschema.JodaTimeSupport._
import org.joda.time.Instant
import json._

object Test {

  case class Event(id: String, timestamp: Instant)
  val eventSchema: Schema[Event] = Json.schema[Event]

  def main(args: Array[String]): Unit = {


    println(JsonFormatter.format(AsValue.schema(eventSchema)))
  }
}
