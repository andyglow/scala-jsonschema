package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import spray.json._
import com.github.andyglow.scalamigration._
import json.schema.Version


object AsSpray {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class SpraySchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asSpray[V <: Version](v: V)(implicit asValue: AsValueBuilder[V]): JsObject = AsSpray(AsValue.schema(x, v))
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
    def unadapt(x: P): T
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, JsValue] = Adapter.make({
      case `null`  => JsNull
      case `true`  => JsTrue
      case `false` => JsFalse
      case x: num  => Adapter.numAdapter.adapt(x)
      case x: str  => Adapter.strAdapter.adapt(x)
      case x: arr  => Adapter.arrAdapter.adapt(x)
      case x: obj  => Adapter.objAdapter.adapt(x)
    }, {
      case JsNull       => `null`
      case JsTrue       => `true`
      case JsFalse      => `false`
      case x: JsNumber  => Adapter.numAdapter.unadapt(x)
      case x: JsString  => Adapter.strAdapter.unadapt(x)
      case x: JsArray   => Adapter.arrAdapter.unadapt(x)
      case x: JsObject  => Adapter.objAdapter.unadapt(x)
    })
  }

  object Adapter extends LowPriorityAdapter {
    type Aux[T, PP] = Adapter[T] { type P = PP }

    def adapt[T, PP](value: T)(implicit a: Aux[T, PP]): PP = a.adapt(value)

    def unadapt[T, PP](value: PP)(implicit a: Aux[T, PP]): T = a.unadapt(value)

    def make[T, PP](to: T => PP, from: PP => T): Aux[T, PP] = new Adapter[T] {
      type P = PP

      def adapt(x: T): PP = to(x)
      def unadapt(x: PP): T = from(x)
    }

    implicit val nullAdapter: Aux[`null`.type, JsNull.type] = make(_ => JsNull, _ => `null`)
    implicit val trueAdapter: Aux[`true`.type, JsBoolean] = make(_ => JsTrue, _ => `true`)
    implicit val falseAdapter: Aux[`false`.type, JsBoolean] = make(_ => JsFalse, _ => `false`)
    implicit val numAdapter: Aux[num, JsNumber] = make(x => JsNumber(x.value), x => num(x.value))
    implicit val strAdapter: Aux[str, JsString] = make(x => JsString(x.value), x => str(x.value))
    implicit val arrAdapter: Aux[arr, JsArray] = make(
      x => JsArray { x.value.toVector map { adapt(_) } },
      x => arr { x.elements map { unadapt(_) }})
    implicit val objAdapter: Aux[obj, JsObject] = make(
      x => JsObject { x.value.toMap mapV { adapt(_) } },
      x => obj { x.fields.toMap mapV { unadapt(_) } })
  }


  implicit def toValue[T](implicit w: JsonWriter[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js = w.write(x)
      Adapter.unadapt(js)
    }
  }
}
