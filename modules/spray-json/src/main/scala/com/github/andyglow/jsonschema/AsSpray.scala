package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import spray.json._
import json.schema.Version
import scala.collection.immutable.ListMap

object AsSpray {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class SpraySchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asSpray[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: _root_.json.schema.JsonSchemaProfile): JsObject = AsSpray(
      AsValue.schema(x, v)
    )
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
    def unadapt(x: P): T
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, JsValue] = Adapter.make(
      {
        case `null`  => JsNull
        case `true`  => JsTrue
        case `false` => JsFalse
        case x: num  => Adapter.numAdapter.adapt(x)
        case x: str  => Adapter.strAdapter.adapt(x)
        case x: arr  => Adapter.arrAdapter.adapt(x)
        case x: obj  => Adapter.objAdapter.adapt(x)
      },
      {
        case JsNull      => `null`
        case JsTrue      => `true`
        case JsFalse     => `false`
        case x: JsNumber => Adapter.numAdapter.unadapt(x)
        case x: JsString => Adapter.strAdapter.unadapt(x)
        case x: JsArray  => Adapter.arrAdapter.unadapt(x)
        case x: JsObject => Adapter.objAdapter.unadapt(x)
      }
    )
  }

  object Adapter extends LowPriorityAdapter {
    type Aux[T, PP] = Adapter[T] { type P = PP }

    def adapt[T, PP](value: T)(implicit a: Aux[T, PP]): PP = a.adapt(value)

    def unadapt[T, PP](value: PP)(implicit a: Aux[T, PP]): T = a.unadapt(value)

    def make[T, PP](to: T => PP, from: PP => T): Aux[T, PP] = new Adapter[T] {
      type P = PP

      def adapt(x: T): PP   = to(x)
      def unadapt(x: PP): T = from(x)
    }

    implicit val nullAdapter: Aux[`null`.type, JsNull.type] = make(_ => JsNull, _ => `null`)
    implicit val trueAdapter: Aux[`true`.type, JsBoolean]   = make(_ => JsTrue, _ => `true`)
    implicit val falseAdapter: Aux[`false`.type, JsBoolean] = make(_ => JsFalse, _ => `false`)
    implicit val numAdapter: Aux[num, JsNumber]             = make(x => JsNumber(x.value), x => num(x.value))
    implicit val strAdapter: Aux[str, JsString]             = make(x => JsString(x.value), x => str(x.value))
    implicit val arrAdapter: Aux[arr, JsArray]              = make(
      x => JsArray { x.value.toVector map { adapt(_) } },
      x => arr { x.elements map { unadapt(_) } }
    )
    implicit val objAdapter: Aux[obj, JsObject] = make(
      // spray's JsObject is a Map; a ListMap keeps the order (its varargs constructor builds a HashMap)
      x => JsObject(ListMap(x.fields.toList map { case (k, v) => (k, adapt(v)) }: _*)),
      x => obj.ordered(x.fields.toList map { case (k, v) => (k, unadapt(v)) }) // toList: on 2.12, `Map.map` builds a HashMap even from a ListMap
    )
  }

  implicit def toValue[T](implicit w: JsonWriter[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js = w.write(x)
      Adapter.unadapt(js)
    }
  }

  /** `import com.github.andyglow.jsonschema.AsSpray.predef._` (Scala 3: `.*`): inlined schemas for spray-json's AST types. `JsValue` is any JSON value (`{}`), `JsObject` a free object, `JsArray` an
    * array of any values. Without it, spray's AST derives as a family of its case classes, which no JSON spray writes matches. Opt-in: these next to a `Predef[JsObject]` of your own would be
    * ambiguous.
    */
  object predef {
    import _root_.json.schema.Predef
    import _root_.json.{Schema => S}

    implicit val sprayJsValuePredef: Predef[JsValue]     = Predef(S.`any`[JsValue]())
    implicit val sprayJsObjectPredef: Predef[JsObject]   = Predef(S.`object`.Free[JsObject]())
    implicit val sprayJsArrayPredef: Predef[JsArray]     = Predef(S.`array`[JsValue, Vector](S.`any`[JsValue]()).asInstanceOf[S[JsArray]])
    implicit val sprayJsStringPredef: Predef[JsString]   = Predef(S.`string`[JsString])
    implicit val sprayJsNumberPredef: Predef[JsNumber]   = Predef(S.`number`[BigDecimal].asInstanceOf[S[JsNumber]])
    implicit val sprayJsBooleanPredef: Predef[JsBoolean] = Predef(S.`boolean`.asInstanceOf[S[JsBoolean]])

    /** `JsNull` (e.g. a field that is always `null`): `{"type": "null"}`. */
    implicit val jsNullPredef: Predef[JsNull.type] = Predef(S.`null`.asInstanceOf[S[JsNull.type]])
  }
}
