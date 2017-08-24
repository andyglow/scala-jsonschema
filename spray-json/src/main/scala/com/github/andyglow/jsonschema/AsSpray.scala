package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import spray.json._

object AsSpray {

  def apply(value: Value): JsValue = value match {
    case `null` => JsNull
    case `true` => JsBoolean(true)
    case `false` => JsBoolean(false)
    case num(x) => JsNumber(x)
    case str(x) => JsString(x)
    case arr(x) => val arr = x map AsSpray.apply; JsArray(arr: _*)
    case obj(x) => val map = x mapValues AsSpray.apply; JsObject(map.toSeq: _*)
  }

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asSpray(
      title: Option[String] = None,
      description: Option[String] = None): JsValue = AsSpray(AsValue.schema(x, title, description))
  }

}
