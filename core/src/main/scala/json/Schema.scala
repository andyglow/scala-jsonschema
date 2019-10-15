package json

import com.github.andyglow.json.{ToValue, Value}

import scala.annotation.implicitNotFound


sealed trait Schema[+T] extends Product {
  import Schema._

  private var _refName: Option[String] = None

  private var _validations: collection.Seq[ValidationDef[_, _]] = Seq.empty

  def jsonType: String = productPrefix

  def withValidation[TT >: T, B](v: ValidationDef[B, _], vs: ValidationDef[B, _]*)(implicit bound: ValidationBound[TT, B]): Schema[T] = {
    this._validations = (v +: vs).foldLeft(_validations) {
      case (agg, v) => bound.append(agg, v)
    }
    this
  }

  def apply(refName: String): Schema[T] = {
    this._refName = Some(refName)
    this
  }

  def refName: Option[String] = _refName

  def validations: Seq[ValidationDef[_, _]] = _validations.toSeq

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(productPrefix)
    if (productIterator.hasNext) {
      sb.append("(")
      sb.append(productIterator.mkString(","))
      sb.append(")")
    }
    refName foreach { refName =>
      sb.append("#")
      sb.append(refName)
    }

    sb.toString
  }
}

object Schema {

  final case object `boolean` extends Schema[Boolean]

  final case object `integer` extends Schema[Int]

  final case class `number`[T : Numeric]() extends Schema[T]

  final case class `string`[T](format: Option[`string`.Format], pattern: Option[String]) extends Schema[T]

  final case class `set`[T, C[_]](componentType: Schema[T]) extends Schema[C[T]] { override def jsonType = "array" }

  final case class `array`[T, C[_]](componentType: Schema[T]) extends Schema[C[T]]

  final case class `string-map`[T](valueType: Schema[T]) extends Schema[Map[String, T]] { override def jsonType = "object" }

  final case class `int-map`[T](valueType: Schema[T]) extends Schema[Map[Int, T]] { override def jsonType = "object" }

  final case class `object`[T](fields: Set[`object`.Field[_]]) extends Schema[T]

  final case class `enum`[T](values: Set[Value]) extends Schema[T]

  final case class `oneof`[T](subTypes: Set[Schema[_]]) extends Schema[T]

  final case class `ref`[T](sig: String, tpe: Schema[T]) extends Schema[T] { override def jsonType: String = s"$$ref" }

  @implicitNotFound("Implicit not found: ValidationBound[${F}, ${T}]. Some of validations doesn't match schema type")
  sealed trait ValidationBound[F, T] {
    def append(
      seq: collection.Seq[ValidationDef[_, _]],
      item: ValidationDef[T, _]): collection.Seq[ValidationDef[_, _]] = seq :+ item
  }
  object ValidationBound {

    implicit def identity[X]: ValidationBound[X, X] = new ValidationBound[X, X] {}

    implicit def numeric[X: Numeric]: ValidationBound[X, Number] = new ValidationBound[X, Number] {}

    implicit def stringMap[X]: ValidationBound[Map[String, X], Map[String, _]] = new ValidationBound[Map[String, X], Map[String, _]] {}
    implicit def map[K, V]: ValidationBound[Map[K, V], Map[_, _]] = new ValidationBound[Map[K, V], Map[_, _]] {}

    implicit def array[X]: ValidationBound[Array[X], Iterable[_]] = new ValidationBound[Array[X], Iterable[_]] {}
    implicit def iterable[X]: ValidationBound[Iterable[X], Iterable[_]] = new ValidationBound[Iterable[X], Iterable[_]] {}
    implicit def seq[X]: ValidationBound[Seq[X], Iterable[_]] = new ValidationBound[Seq[X], Iterable[_]] {}
    implicit def list[X]: ValidationBound[List[X], Iterable[_]] = new ValidationBound[List[X], Iterable[_]] {}
    implicit def vector[X]: ValidationBound[Vector[X], Iterable[_]] = new ValidationBound[Vector[X], Iterable[_]] {}
    implicit def set[X]: ValidationBound[Set[X], Iterable[_]] = new ValidationBound[Set[X], Iterable[_]] {}
  }

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

  final object `object` {

    final class Field[T](
      val name: String,
      val tpe: Schema[T],
      val required: Boolean,
      val default: Option[Value]) {

      def canEqual(that: Any): Boolean = that.isInstanceOf[Field[T]]

      override def equals(that: Any): Boolean = canEqual(that) && {
        val other = that.asInstanceOf[Field[T]]

        this.name     == other.name &&
        this.required == other.required &&
        this.tpe      == other.tpe &&
        this.default  == other.default
      }

      override def hashCode: Int = name.hashCode

      override def toString: String = {
        val extra = (required, default) match {
          case (true, None)     => " /R"
          case (false, None)    => ""
          case (true, Some(v))  => s" /R /$v"
          case (false, Some(v)) => s" /$v"
        }

        s"$name: ${tpe}$extra"
      }
    }

    final object Field {

      def apply[T](
        name: String,
        tpe: Schema[T]): Field[T] = {

        new Field(name, tpe, required = true, default = None)
      }

      def apply[T](
        name: String,
        tpe: Schema[T],
        required: Boolean): Field[T] = {

        new Field(name, tpe, required, default = None)
      }

      def apply[T: ToValue](
        name: String,
        tpe: Schema[T],
        required: Boolean,
        default: T): Field[T] = {

        new Field(name, tpe, required, Some(ToValue(default)))
      }

      def fromJson[T](
        name: String,
        tpe: Schema[T],
        required: Boolean,
        default: Option[Value]): Field[T] = {

        new Field(name, tpe, required, default)
      }
    }

    def apply[T](field: Field[_], xs: Field[_]*): `object`[T] = new `object`((field +: xs.toSeq).toSet)
  }
}