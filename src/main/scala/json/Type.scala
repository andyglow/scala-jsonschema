package json

sealed trait Type extends Product

object Type {

  case object `boolean` extends Type

  case object `integer` extends Type

  case object `number` extends Type

  case class `string`(format: Option[`string`.Format], pattern: Option[String]) extends Type

  object `string` {

    private val instance = `string`(None, None)

    def apply(): `string` = instance

    def apply(format: Format): `string` = `string`(Some(format), None)

    def apply(pattern: String): `string` = `string`(None, Some(pattern))

    trait Format extends Product

    object Format {

      case object `date` extends Format

      case object `time` extends Format

      case object `date-time` extends Format // Date representation, as defined by RFC 3339, section 5.6.

      case object `email` extends Format // Internet email address, see RFC 5322, section 3.4.1.

      case object `hostname` extends Format // Internet host name, see RFC 1034, section 3.1.

      case object `ipv4` extends Format // Internet host name, see RFC 1034, section 3.1.

      case object `ipv6` extends Format // IPv6 address, as defined in RFC 2373, section 2.2.

      case object `uri` extends Format // A universal resource identifier (URI), according to RFC3986.
    }
  }

  case class `array`(schema: Schema) extends Type

  case class `object`(fields: Set[`object`.Field]) extends Type

  object `object` {

    case class Field(name: String, schema: Schema, required: Boolean = true) {

      def canEqual(that: Any): Boolean = that.isInstanceOf[Field]

      override def equals(that: Any): Boolean = canEqual(that) && (this.name == that.asInstanceOf[Field].name)

      override def hashCode: Int = name.hashCode
    }

    def apply(field: Field, xs: Field*): `object` = new `object`((field +: xs.toSeq).toSet)
  }
}




