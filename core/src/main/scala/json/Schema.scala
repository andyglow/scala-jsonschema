package json

import scala.language.higherKinds

sealed trait Schema[+T] extends Product {

  type BoundType

  private var _refName: Option[String] = None

  private var _validations: Seq[ValidationDef[BoundType, _]] = Seq.empty

  def jsonType: String = productPrefix

  def withValidation(v: ValidationDef[BoundType, _], vs: ValidationDef[BoundType, _]*): Schema[T] = {
    this._validations = v +: vs
    this
  }

  def apply(refName: String): Schema[T] = {
    this._refName = Some(refName)
    this
  }

  def refName: Option[String] = _refName

  def validations: Seq[ValidationDef[BoundType, _]] = _validations
}

object Schema {

  final case object `boolean` extends Schema[Boolean] { override type BoundType = Boolean }

  final case object `integer` extends Schema[Int] { override type BoundType = Int }

  final case class `number`[T : Numeric]() extends Schema[T] { override type BoundType = Number }

  final case class `string`[T](format: Option[`string`.Format], pattern: Option[String]) extends Schema[T] { override type BoundType = String }

  object `string` {

    trait Format extends Product

    object Format {

      final case object `date` extends Format

      final case object `time` extends Format

      final case object `date-time` extends Format // Date representation, as defined by RFC 3339, section 5.6.

      final case object `email` extends Format // Internet email address, see RFC 5322, section 3.4.1.

      final case object `hostname` extends Format // Internet host name, see RFC 1034, section 3.1.

      final case object `ipv4` extends Format // Internet host name, see RFC 1034, section 3.1.

      final case object `ipv6` extends Format // IPv6 address, as defined in RFC 2373, section 2.2.

      final case object `uri` extends Format // A universal resource identifier (URI), according to RFC3986.
    }
  }

  final case class `set`[T, C[_]](componentType: Schema[T]) extends Schema[C[T]] { override type BoundType = Set[_]; override def jsonType = "array" }

  final case class `array`[T, C[_]](componentType: Schema[T]) extends Schema[C[T]] { override type BoundType = Traversable[_] }

  final case class `string-map`[T](valueType: Schema[T]) extends Schema[Map[String, T]] { override type BoundType = Map[String, _]; override def jsonType = "object" }

  final case class `int-map`[T](valueType: Schema[T]) extends Schema[Map[Int, T]] { override type BoundType = Map[Int, _]; override def jsonType = "object" }

  final case class `object`[T](fields: Set[`object`.Field[_]]) extends Schema[T]

  object `object` {

    final case class Field[T](name: String, tpe: Schema[T], required: Boolean = true) {

      def canEqual(that: Any): Boolean = that.isInstanceOf[Field[T]]

      override def equals(that: Any): Boolean = canEqual(that) && {
        val other = that.asInstanceOf[Field[T]]

        this.name     == other.name &&
        this.required == other.required &&
        this.tpe      == other.tpe
      }

      override def hashCode: Int = name.hashCode
    }

    def apply[T](field: Field[_], xs: Field[_]*): `object`[T] = new `object`((field +: xs.toSeq).toSet)
  }

  final case class `enum`[T](values: Set[String]) extends Schema[T] { override type BoundType = T }

  final case class `oneof`[T](subTypes: Set[`object`[_]]) extends Schema[T] { override type BoundType = T }

  final case class $ref[T](sig: String, tpe: Schema[T]) extends Schema[T] { override type BoundType = T }
}