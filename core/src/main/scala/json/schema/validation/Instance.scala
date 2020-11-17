package json.schema.validation

import com.github.andyglow.json.Value._
import com.github.andyglow.json.Value
import com.github.andyglow.jsonschema.AsValue
import json.schema.Version.Draft04

sealed abstract class Instance[S, V]()(implicit conv: V => Value) extends Product {

  def name: String = productPrefix

  def :=(x: V): Def[S, V] = Def(this, x, x)
}

object Instance {

  final case object `multipleOf` extends Instance[Number, Number]()(num.apply)

  final case object `maximum` extends Instance[Number, Number]()(num.apply)

  final case object `minimum` extends Instance[Number, Number]()(num.apply)

  final case object `exclusiveMaximum` extends Instance[Number, Number]()(num.apply)

  final case object `exclusiveMinimum` extends Instance[Number, Number]()(num.apply)

  final case object `contentEncoding` extends Instance[String, String]()(str.apply)

  final case object `contentMediaType` extends Instance[String, String]()(str.apply)

  final case object `contentSchema` extends Instance[String, json.Schema[_]]()(AsValue.schema(_, Draft04())) // FIXME: shouldn't specify a Version at this level

  final case object `maxLength` extends Instance[String, Int]()(num.apply)

  final case object `minLength` extends Instance[String, Int]()(num.apply)

  final case object `pattern` extends Instance[String, String]()(str.apply)

  final case object `maxItems` extends Instance[Iterable[_], Int]()(num.apply)

  final case object `minItems` extends Instance[Iterable[_], Int]()(num.apply)

  final case object `maxContains` extends Instance[Iterable[_], Int]()(num.apply)

  final case object `minContains` extends Instance[Iterable[_], Int]()(num.apply)

  final case object `uniqueItems` extends Instance[Iterable[_], Boolean]()(bool.apply)

  final case object `maxProperties` extends Instance[Map[_, _], Int]()(num.apply)

  final case object `minProperties` extends Instance[Map[_, _], Int]()(num.apply)

  final case object `patternProperties` extends Instance[Map[_, _], String]()(str.apply)
}

