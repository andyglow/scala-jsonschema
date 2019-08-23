package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import spray.json._
import com.github.andyglow.scalamigration._
import json.schema.Version


object AsSpray {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asSpray[V <: Version](v: V)(implicit asValue: AsValueBuilder[V]): JsObject = AsSpray(AsValue.schema(x, v))
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, JsValue] = Adapter make {
      case `null`  => JsNull
      case `true`  => JsTrue
      case `false` => JsFalse
      case x: num  => Adapter.numAdapter.adapt(x)
      case x: str  => Adapter.strAdapter.adapt(x)
      case x: arr  => Adapter.arrAdapter.adapt(x)
      case x: obj  => Adapter.objAdapter.adapt(x)
    }
  }

  object Adapter extends LowPriorityAdapter {
    type Aux[T, PP] = Adapter[T] { type P = PP }

    def make[T, PP](f: T => PP): Aux[T, PP] = new Adapter[T] {
      type P = PP

      def adapt(x: T): PP = f(x)
    }

    implicit val nullAdapter: Aux[`null`.type, JsNull.type] = make(_ => JsNull)
    implicit val trueAdapter: Aux[`true`.type, JsBoolean] = make(_ => JsTrue)
    implicit val falseAdapter: Aux[`false`.type, JsBoolean] = make(_ => JsFalse)
    implicit val numAdapter: Aux[num, JsNumber] = make(x => JsNumber(x.value))
    implicit val strAdapter: Aux[str, JsString] = make(x => JsString(x.value))
    implicit val arrAdapter: Aux[arr, JsArray] = make(x => JsArray { x.value.toVector map { AsSpray(_) } })
    implicit val objAdapter: Aux[obj, JsObject] = make(x => JsObject { x.value.toMap mapV { AsSpray(_) } })
  }
}
