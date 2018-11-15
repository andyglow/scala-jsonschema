package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.json._
import json.Schema
import org.json4s.JsonAST._

object AsJson4s {

  def apply(value: Value): JValue = value match {
    case `null` => JNull
    case `true` => JBool(true)
    case `false` => JBool(false)
    case num(x) => JDecimal(x)
    case str(x) => JString(x)
    case arr(x) => JArray(x.map(AsJson4s.apply).toList)
    case obj(x) => JObject(x.map { case (k, v) => JField(k, AsJson4s.apply(v)) }.toList)
  }

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asJson4s(
      title: Option[String] = None,
      description: Option[String] = None): JValue = AsJson4s(AsValue.schema(x, title, description))
  }
}
