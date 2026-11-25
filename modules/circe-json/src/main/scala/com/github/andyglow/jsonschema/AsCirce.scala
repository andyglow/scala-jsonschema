package com.github.andyglow.jsonschema

import com.github.andyglow.json._
import com.github.andyglow.json.Value._
import json.Schema
import io.circe._
import json.schema.Version

object AsCirce {

  final def apply(value: Value): Json = value match {
    case `null`  => Json.Null
    case `true`  => Json.True
    case `false` => Json.False
    case num(x)  => Json.fromBigDecimal(x)
    case str(x)  => Json.fromString(x)
    case arr(x)  => val arr = x map AsCirce.apply; Json.arr(arr.toSeq: _*)
    case x: obj  => Json.obj(x.fields.toList.map { case (k, v) => (k, AsCirce(v)) }: _*)
  }

  implicit class CirceSchemaOps[T](val x: Schema[T]) extends AnyVal {

    def asCirce[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: _root_.json.schema.JsonSchemaProfile): Json = AsCirce(
      AsValue.schema(x, v)
    )
  }

  implicit def toValue[T](implicit w: Encoder[T]): ToValue[T] = new ToValue[T] {
    override def apply(x: T): Value = {
      val js                         = w.apply(x)
      def translate(js: Json): Value = js match {
        case Json.Null       => `null`
        case Json.True       => `true`
        case Json.False      => `false`
        case x if x.isNumber => num(x.asNumber.get.toDouble)
        case x if x.isString => str(x.asString.get)
        case x if x.isArray  => val a = x.asArray.get map translate; arr(a)
        case x if x.isObject => obj.ordered(x.asObject.get.toList map { case (k, v) => (k, translate(v)) })
        case _               => throw new IllegalArgumentException() // compiler warns of non-exhaustive matching otherwise
      }

      translate(js)
    }
  }

  /** `import com.github.andyglow.jsonschema.AsCirce.predef._` (Scala 3: `.*`): inlined schemas for circe's AST types. `Json` is any JSON value (`{}`), `JsonObject` a free object, `JsonNumber` a
    * number. Opt-in: these next to a `Predef[JsonObject]` of your own would be ambiguous.
    */
  object predef {
    import _root_.json.schema.Predef
    import _root_.json.{Schema => S}

    implicit val circeJsonPredef: Predef[Json]             = Predef(S.`any`[Json]())
    implicit val circeJsonObjectPredef: Predef[JsonObject] = Predef(S.`object`.Free[JsonObject]())
    implicit val circeJsonNumberPredef: Predef[JsonNumber] = Predef(S.`number`[BigDecimal].asInstanceOf[S[JsonNumber]])
  }
}
