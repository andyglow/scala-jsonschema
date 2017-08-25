package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import play.api.libs.json._

object AsPlay {

  def apply(value: Value): JsValue = value match {
    case `null` => JsNull
    case `true` => JsBoolean(true)
    case `false` => JsBoolean(false)
    case num(x) => JsNumber(x)
    case str(x) => JsString(x)
    case arr(x) => JsArray(x map AsPlay.apply)
    case obj(x) => JsObject(x mapValues AsPlay.apply)
  }

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asPlay(
      title: Option[String] = None,
      description: Option[String] = None): JsValue = AsPlay(AsValue.schema(x, title, description))
  }
}
