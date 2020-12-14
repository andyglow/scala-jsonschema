package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import com.github.andyglow.scalamigration._
import json.schema.Version
import upickle.default._


object AsU {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class UJsonSchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asU[V <: Version](v: V)(implicit asValue: AsValueBuilder[V]): ujson.Obj = AsU(AsValue.schema(x, v))
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
    def unadapt(x: P): T
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, ujson.Value] = Adapter.make({
      case `null`  => ujson.Null
      case `true`  => ujson.True
      case `false` => ujson.False
      case x: num  => Adapter.numAdapter.adapt(x)
      case x: str  => Adapter.strAdapter.adapt(x)
      case x: arr  => Adapter.arrAdapter.adapt(x)
      case x: obj  => Adapter.objAdapter.adapt(x)
    }, {
      case ujson.Null => `null`
      case ujson.True => `true`
      case ujson.False => `false`
      case x: ujson.Num  => Adapter.numAdapter.unadapt(x)
      case x: ujson.Str  => Adapter.strAdapter.unadapt(x)
      case x: ujson.Arr  => Adapter.arrAdapter.unadapt(x)
      case x: ujson.Obj  => Adapter.objAdapter.unadapt(x)
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

    implicit val nullAdapter: Aux[`null`.type, ujson.Null.type] = make(_ => ujson.Null, _ => `null`)
    implicit val trueAdapter: Aux[`true`.type, ujson.True.type] = make(_ => ujson.True, _ => `true`)
    implicit val falseAdapter: Aux[`false`.type, ujson.False.type] = make(_ => ujson.False, _ => `false`)
    implicit val numAdapter: Aux[num, ujson.Num] = make(x => ujson.Num(x.value.toDouble), x => num(x.value))
    implicit val strAdapter: Aux[str, ujson.Str] = make(x => ujson.Str(x.value), x => str(x.value))
    implicit val arrAdapter: Aux[arr, ujson.Arr] = make(x => x.value map { adapt(_) }, x => arr { x.value map { unadapt(_) }})
    implicit val objAdapter: Aux[obj, ujson.Obj] = make(x => x.fields.toList mapV { adapt(_) }, x => obj { x.value.toList mapV { unadapt(_) }})
  }

  implicit def toValue[T](implicit w: Writer[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js = writeJs(x)
      Adapter.unadapt(js)
    }
  }
}
