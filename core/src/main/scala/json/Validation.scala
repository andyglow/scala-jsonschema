package json

import com.github.andyglow.json.Value._
import com.github.andyglow.json.Value

case class ValidationDef[T](name: String, value: T, json: Value)

sealed abstract class Validation[T]()(implicit conv: T => Value) extends Product {

  def :=(x: T): ValidationDef[T] = ValidationDef(productPrefix, x, x)
}

object Validation {

  case object `multipleOf` extends Validation[Int]()(num.apply)

  case object `maximum` extends Validation[Number]()(num.apply)

  case object `minimum` extends Validation[Number]()(num.apply)

  case object `exclusiveMaximum` extends Validation[Number]()(num.apply)

  case object `exclusiveMinimum` extends Validation[Number]()(num.apply)

  case object `maxLength` extends Validation[Int]()(num.apply)

  case object `minLength` extends Validation[Int]()(num.apply)

  case object `pattern` extends Validation[String]()(str.apply)

  case object `maxItems` extends Validation[Int]()(num.apply)

  case object `minItems` extends Validation[Int]()(num.apply)

  case object `uniqueItems` extends Validation[Boolean]()(bool.apply)

  case object `maxProperties` extends Validation[Int]()(num.apply)

  case object `minProperties` extends Validation[Int]()(num.apply)
}


