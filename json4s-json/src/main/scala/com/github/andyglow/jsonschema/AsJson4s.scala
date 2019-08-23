package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.json._
import json.Schema
import json.schema.Version
import org.json4s.JsonAST._

object AsJson4s {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asJson4s[V <: Version](v: V)(implicit asValue: AsValueBuilder[V]): JObject =
      AsJson4s(AsValue.schema(x, v))
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, JValue] = Adapter make {
      case `null`  => JNull
      case `true`  => JBool.True
      case `false` => JBool.False
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

    implicit val nullAdapter: Aux[`null`.type, JNull.type] = make(_ => JNull)
    implicit val trueAdapter: Aux[`true`.type, JBool] = make(_ => JBool.True)
    implicit val falseAdapter: Aux[`false`.type, JBool] = make(_ => JBool.False)
    implicit val numAdapter: Aux[num, JDecimal] = make(x => JDecimal(x.value))
    implicit val strAdapter: Aux[str, JString] = make(x => JString(x.value))
    implicit val arrAdapter: Aux[arr, JArray] = make(x => JArray { x.value.toList map { AsJson4s(_) } })
    implicit val objAdapter: Aux[obj, JObject] = make { x =>
      val fields = x.value.toList.map {
        case (k, v) => JField(k, AsJson4s.apply(v))
      }

      JObject(fields)
    }
  }
}
