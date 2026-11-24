package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import com.github.andyglow.json._
import json.Schema
import json.schema.Version
import org.json4s.JsonAST._
import com.github.andyglow.scalamigration._
import org.json4s.Writer

object AsJson4s {

  def apply[T](value: T)(implicit a: Adapter[T]): a.P = a.adapt(value)

  implicit class Json4sSchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asJson4s[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: _root_.json.schema.JsonSchemaProfile): JObject =
      AsJson4s(AsValue.schema(x, v))
  }

  trait Adapter[T] {
    type P

    def adapt(x: T): P
    def unadapt(x: P): T
  }

  trait LowPriorityAdapter {

    implicit val anyAdapter: Adapter.Aux[Value, JValue] = Adapter.make(
      {
        case `null`  => JNull
        case `true`  => JBool.True
        case `false` => JBool.False
        case x: num  => Adapter.numAdapter.adapt(x)
        case x: str  => Adapter.strAdapter.adapt(x)
        case x: arr  => Adapter.arrAdapter.adapt(x)
        case x: obj  => Adapter.objAdapter.adapt(x)
      },
      {
        case JNothing    => `null`
        case JNull       => `null`
        case JBool(x)    => bool(x)
        case JDouble(x)  => num(x)
        case JDecimal(x) => num(x)
        case JLong(x)    => num(x)
        case JInt(x)     => num(x)
        case JString(x)  => str(x)
        case JSet(x)     => Adapter.arrAdapter.unadapt(JArray(x.toList))
        case x: JArray   => Adapter.arrAdapter.unadapt(x)
        case x: JObject  => Adapter.objAdapter.unadapt(x)
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

    implicit val nullAdapter: Aux[`null`.type, JNull.type] = make(_ => JNull, _ => `null`)
    implicit val trueAdapter: Aux[`true`.type, JBool]      = make(_ => JBool.True, _ => `true`)
    implicit val falseAdapter: Aux[`false`.type, JBool]    = make(_ => JBool.False, _ => `false`)
    implicit val numAdapter: Aux[num, JDecimal]            = make(x => JDecimal(x.value), x => num(x.values))
    implicit val strAdapter: Aux[str, JString]             = make(x => JString(x.value), x => str(x.values))
    implicit val arrAdapter: Aux[arr, JArray]              =
      make(x => JArray { x.value.toList map { adapt(_) } }, x => arr { x.arr map { unadapt(_) } })
    implicit val objAdapter: Aux[obj, JObject] = make(
      { x =>
        val fields = x.value.toList.map { case (k, v) =>
          JField(k, AsJson4s.apply(v))
        }

        JObject(fields)
      },
      { x =>
        obj { x.obj.toMap mapV { unadapt(_) } }
      }
    )
  }

  implicit def toValue[T](implicit w: Writer[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js = w.write(x)
      Adapter.unadapt(js)
    }
  }

  /** `import com.github.andyglow.jsonschema.AsJson4s.predef._` (Scala 3: `.*`): inlined schemas for json4s' AST types. `JValue` is any JSON value (`{}`), `JObject` a free object, `JArray` an array of
    * any values (`JSet`: unique). Without it, json4s' AST derives as a family of its case classes, which no JSON json4s writes matches. Opt-in: these next to a `Predef[JObject]` of your own would be
    * ambiguous. json4s omits a field whose value is `JNothing`: make such a field optional or give it a default.
    */
  object predef {
    import _root_.json.schema.Predef
    import _root_.json.{Schema => S}
    import org.json4s

    implicit val json4sJValuePredef: Predef[json4s.JValue]     = Predef(S.`any`[json4s.JValue]())
    implicit val json4sJObjectPredef: Predef[json4s.JObject]   = Predef(S.`object`.Free[json4s.JObject]())
    implicit val json4sJArrayPredef: Predef[json4s.JArray]     = Predef(S.`array`[json4s.JValue, List](S.`any`[json4s.JValue]()).asInstanceOf[S[json4s.JArray]])
    implicit val json4sJSetPredef: Predef[json4s.JSet]         = Predef(S.`array`[json4s.JValue, Set](S.`any`[json4s.JValue](), unique = true).asInstanceOf[S[json4s.JSet]])
    implicit val json4sJStringPredef: Predef[json4s.JString]   = Predef(S.`string`[json4s.JString])
    implicit val json4sJNumberPredef: Predef[json4s.JNumber]   = Predef(S.`number`[BigDecimal].asInstanceOf[S[json4s.JNumber]])
    implicit val json4sJDoublePredef: Predef[json4s.JDouble]   = Predef(S.`number`[Double].asInstanceOf[S[json4s.JDouble]])
    implicit val json4sJDecimalPredef: Predef[json4s.JDecimal] = Predef(S.`number`[BigDecimal].asInstanceOf[S[json4s.JDecimal]])
    implicit val json4sJLongPredef: Predef[json4s.JLong]       = Predef(S.`number`[Long].asInstanceOf[S[json4s.JLong]])
    implicit val json4sJIntPredef: Predef[json4s.JInt]         = Predef(S.`number`[BigInt].asInstanceOf[S[json4s.JInt]])
    implicit val json4sJBoolPredef: Predef[json4s.JBool]       = Predef(S.`boolean`.asInstanceOf[S[json4s.JBool]])
  }
}
