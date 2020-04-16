package json

import com.github.andyglow.json.Value._
import com.github.andyglow.json.Value
import com.github.andyglow.jsonschema.AsValue
import json.schema.Version.Draft04


final case class ValidationDef[S, V](validation: Validation[S, V], value: V, json: Value) {

  override def canEqual(that: Any): Boolean = that.isInstanceOf[ValidationDef[_, _]]

  override def equals(that: Any): Boolean = that match {
    case that: ValidationDef[_, _] => getClass == that.getClass && validation == that.validation && json == that.json
    case _ => false
  }
}

sealed abstract class Validation[S, V]()(implicit conv: V => Value) extends Product {

  def name: String = productPrefix

  def :=(x: V): ValidationDef[S, V] = ValidationDef(this, x, x)
}

object Validation {

  final case object `multipleOf` extends Validation[Number, Number]()(num.apply)

  final case object `maximum` extends Validation[Number, Number]()(num.apply)

  final case object `minimum` extends Validation[Number, Number]()(num.apply)

  final case object `exclusiveMaximum` extends Validation[Number, Number]()(num.apply)

  final case object `exclusiveMinimum` extends Validation[Number, Number]()(num.apply)

  final case object `contentEncoding` extends Validation[String, String]()(str.apply)

  final case object `contentMediaType` extends Validation[String, String]()(str.apply)

  final case object `contentSchema` extends Validation[String, Schema[_]]()(AsValue.schema(_, Draft04())) // FIXME: shouldn't specify a Version at this level

  final case object `maxLength` extends Validation[String, Int]()(num.apply)

  final case object `minLength` extends Validation[String, Int]()(num.apply)

  final case object `pattern` extends Validation[String, String]()(str.apply)

  final case object `maxItems` extends Validation[Iterable[_], Int]()(num.apply)

  final case object `minItems` extends Validation[Iterable[_], Int]()(num.apply)

  final case object `maxContains` extends Validation[Iterable[_], Int]()(num.apply)

  final case object `minContains` extends Validation[Iterable[_], Int]()(num.apply)

  final case object `uniqueItems` extends Validation[Iterable[_], Boolean]()(bool.apply)

  final case object `maxProperties` extends Validation[Map[_, _], Int]()(num.apply)

  final case object `minProperties` extends Validation[Map[_, _], Int]()(num.apply)

  final case object `patternProperties` extends Validation[Map[_, _], String]()(str.apply)
}


