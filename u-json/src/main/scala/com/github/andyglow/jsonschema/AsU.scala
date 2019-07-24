package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import com.github.andyglow.scalamigration._


object AsU {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class SchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asU(
      title: Option[String] = None,
      description: Option[String] = None): ujson.Obj = AsU(AsValue.schema(x, title, description))
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, ujson.Value] = Adapter make {
      case `null`  => ujson.Null
      case `true`  => ujson.True
      case `false` => ujson.False
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

    implicit val nullAdapter: Aux[`null`.type, ujson.Null.type] = make(_ => ujson.Null)
    implicit val trueAdapter: Aux[`true`.type, ujson.True.type] = make(_ => ujson.True)
    implicit val falseAdapter: Aux[`false`.type, ujson.False.type] = make(_ => ujson.False)
    implicit val numAdapter: Aux[num, ujson.Num] = make(x => ujson.Num(x.value.toDouble))
    implicit val strAdapter: Aux[str, ujson.Str] = make(x => ujson.Str(x.value))
    implicit val arrAdapter: Aux[arr, ujson.Arr] = make(x => x.value map { AsU(_) })
    implicit val objAdapter: Aux[obj, ujson.Obj] = make(x => x.value.toMap mapV { AsU(_) })
  }
}
