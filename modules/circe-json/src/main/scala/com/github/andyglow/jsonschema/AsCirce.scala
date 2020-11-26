package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import io.circe._
import com.github.andyglow.scalamigration._
import json.schema.Version


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

  implicit class CirceSchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asCirce[V <: Version](v: V)(implicit asValue: AsValueBuilder[V]): Json = AsCirce(AsValue.schema(x, v))
  }

  implicit def toValue[T](implicit w: Encoder[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js = w.apply(x)
      def translate(js: Json): Value = js match {
        case Json.Null => `null`
        case Json.True => `true`
        case Json.False=> `false`
        case x if x.isNumber => num(x.asNumber.get.toDouble)
        case x if x.isString => str(x.asString.get)
        case x if x.isArray  => val a = x.asArray.get map translate; arr(a)
        case x if x.isObject => val map = x.asObject.get.toMap mapV translate; obj(map)
      }

      translate(js)
    }
  }
}
