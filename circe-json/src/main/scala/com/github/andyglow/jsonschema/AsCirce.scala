package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import io.circe._
import com.github.andyglow.scalamigration._


object AsCirce {

  final def apply(value: Value): Json = value match {
    case `null` => Json.Null
    case `true` => Json.True
    case `false` => Json.False
    case num(x) => Json.fromBigDecimal(x)
    case str(x) => Json.fromString(x)
    case arr(x) => val arr = x map AsCirce.apply; Json.arr(arr.toSeq: _*)
    case obj(x) => val map = x.toMap mapV AsCirce.apply; Json.obj(map.toSeq: _*)
  }

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asCirce(
      title: Option[String] = None,
      description: Option[String] = None): Json = AsCirce(AsValue.schema(x, title, description))
  }
}
