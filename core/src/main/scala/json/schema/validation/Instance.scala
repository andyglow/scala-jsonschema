package json.schema.validation

import com.github.andyglow.json.Value._
import com.github.andyglow.json.Value
import com.github.andyglow.jsonschema.AsValue
import json.schema.Version.Draft04

sealed abstract class Instance[S, V]()(implicit conv: V => Value) extends Product {

  def name: String = productPrefix

  def :=(x: V): Def[S, V] = Def(this, x, conv(x))
}

object Instance {

  case object `multipleOf` extends Instance[Number, Number]()(num.apply)

  case object `maximum` extends Instance[Number, Number]()(num.apply)

  case object `minimum` extends Instance[Number, Number]()(num.apply)

  case object `exclusiveMaximum` extends Instance[Number, Number]()(num.apply)

  case object `exclusiveMinimum` extends Instance[Number, Number]()(num.apply)

  case object `contentEncoding` extends Instance[String, String]()(str.apply)

  case object `contentMediaType` extends Instance[String, String]()(str.apply)

  case object `contentSchema` extends Instance[String, json.Schema[_]]()(AsValue.schema(_, Draft04())) // FIXME: shouldn't specify a Version at this level

  case object `maxLength` extends Instance[String, Int]()(num.apply)

  case object `minLength` extends Instance[String, Int]()(num.apply)

  case object `pattern` extends Instance[String, String]()(str.apply)

  case object `maxItems` extends Instance[Iterable[_], Int]()(num.apply)

  case object `minItems` extends Instance[Iterable[_], Int]()(num.apply)

  case object `maxContains` extends Instance[Iterable[_], Int]()(num.apply)

  case object `minContains` extends Instance[Iterable[_], Int]()(num.apply)

  case object `uniqueItems` extends Instance[Iterable[_], Boolean]()(bool.apply)

  case object `maxProperties` extends Instance[Map[_, _], Int]()(num.apply)

  case object `minProperties` extends Instance[Map[_, _], Int]()(num.apply)

  case object `patternProperties` extends Instance[Map[_, _], String]()(str.apply)
}

