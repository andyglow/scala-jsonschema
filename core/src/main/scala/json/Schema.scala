package json

import com.github.andyglow.json.{ToValue, Value}

import scala.annotation.implicitNotFound


sealed trait Schema[+T] extends Product {
  import Schema._

  private var _refName: Option[String] = None

  private var _validations: collection.Seq[ValidationDef[_, _]] = Seq.empty

  def jsonType: String = productPrefix

  def withValidation[TT >: T, B](v: ValidationDef[B, _], vs: ValidationDef[B, _]*)(implicit bound: ValidationBound[TT, B]): Schema[T] = {
    val copy = this.copy()
    copy._validations = (v +: vs).foldLeft(_validations) {
      case (agg, v) => bound.append(agg, v)
    }
    copy
  }

  def apply(refName: String): Schema[T] = {
    val copy = this.copy()
    copy._refName = Some(refName)
    copy
  }

  def refName: Option[String] = _refName

  // NOTE: `.toSeq` is required for scala 2.13
  // otherwise we'll see
  // type mismatch;
  //  [error]  found   : Seq[json.ValidationDef[_, _]] (in scala.collection)
  //  [error]  required: Seq[json.ValidationDef[_, _]] (in scala.collection.immutable)
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
    if (validations.nonEmpty) {
      sb.append(" {")
      var f = true
      validations foreach { v =>
        if (!f) sb.append(", ")
        sb.append(v.validation)
        sb.append(":=")
        sb.append(v.json)
        f = false
      }
      sb.append("}")
    }

    sb.toString
  }

  protected def mkCopy(): Schema[T]

  def copy(): Schema[T] = {
    val copy = mkCopy()
    copy._refName = this._refName
    copy._validations = this._validations

    copy
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Schema[_]] // && getClass == that.getClass

  override def equals(obj: Any): Boolean = obj match {
    case s: Schema[_] =>
      s.canEqual(this) &&
      this.refName == s.refName &&
      // compare collections disregarding order
      this.validations.forall(s.validations.contains) &&
      s.validations.forall(this.validations.contains)

    case _ => false
  }
}

object Schema {

  sealed case class `boolean`() extends Schema[Boolean] {
    def mkCopy() = new `boolean`()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`boolean`]
  }
  final object `boolean` extends `boolean`()

  sealed case class `integer`() extends Schema[Int] {
    def mkCopy() = new `integer`()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`integer`]
  }
  final object `integer` extends `integer`

  final case class `number`[T: Numeric]() extends Schema[T] {
    def mkCopy() = new `number`[T]()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`number`[_]]
  }

  final case class `string`[T](
    format: Option[`string`.Format],
    pattern: Option[String]) extends Schema[T] {
    def mkCopy() = new `string`[T](format, pattern)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`string`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `string`(f, p) => format == f && pattern == p && super.equals(obj)
      case _ => false
    }
  }

  final case class `set`[T, C[_]](
    componentType: Schema[T]) extends Schema[C[T]] {
    override def jsonType = "array"
    def mkCopy() = new `set`[T, C](componentType)
    override def canEqual(that: Any): Boolean = that match {
      case `set`(_) => true
      case _ => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `set`(c) => componentType == c && super.equals(obj)
      case _ => false
    }
  }

  final case class `array`[T, C[_]](
    componentType: Schema[T]) extends Schema[C[T]] {
    def mkCopy() = new `array`[T, C](componentType)
    override def canEqual(that: Any): Boolean = that match {
      case `array`(_) => true
      case _ => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `array`(c) => componentType == c && super.equals(obj)
      case _ => false
    }
  }

  final case class `string-map`[T, C[_ <: String, _]](
    valueType: Schema[T]) extends Schema[C[String, T]] {
    override def jsonType = "object"
    def mkCopy() = new `string-map`[T, C](valueType)
    override def canEqual(that: Any): Boolean = that match {
      case `string-map`(_) => true
      case _ => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `string-map`(c) => valueType == c && super.equals(obj)
      case _ => false
    }
  }

  final case class `int-map`[T, C[_ <: Int, _]](
    valueType: Schema[T]) extends Schema[C[Int, T]] {
    override def jsonType = "object"
    def mkCopy() = new `int-map`[T, C](valueType)
    override def canEqual(that: Any): Boolean = that match {
      case `int-map`(_) => true
      case _ => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `int-map`(c) => valueType == c && super.equals(obj)
      case _ => false
    }
  }

  final case class `object`[T](
    fields: Set[`object`.Field[_]]) extends Schema[T] {
    def mkCopy() = new `object`[T](fields)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`object`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `object`(f) => fields == f && super.equals(obj)
      case _ => false
    }
  }

  final case class `enum`[T](
    values: Set[Value]) extends Schema[T] {
    def mkCopy() = new `enum`[T](values)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`enum`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `enum`(v) => values == v && super.equals(obj)
      case _ => false
    }
  }

  final case class `oneof`[T](
    subTypes: Set[Schema[_]]) extends Schema[T] {
    def mkCopy() = new `oneof`[T](subTypes)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`oneof`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `oneof`(s) => subTypes == s && super.equals(obj)
      case _ => false
    }
  }

  final case class `allof`[T](
    subTypes: Set[Schema[_]]) extends Schema[T] {
    override def jsonType: String = subTypes.head.jsonType
    def mkCopy() = new `allof`[T](subTypes)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`allof`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `allof`(s) => subTypes == s && super.equals(obj)
      case _ => false
    }
  }

  final case class `not`[T](
    tpe: Schema[T]) extends Schema[T] {
    override def jsonType: String = tpe.jsonType
    def mkCopy() = new `not`[T](tpe)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[`not`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `not`(t) => tpe == t && super.equals(obj)
      case _ => false
    }
  }

  final case class `ref`[T](
    sig: String,
    tpe: Schema[_]) extends Schema[T] {
    override def jsonType: String = s"$$ref"
    def mkCopy() = new `ref`[T](sig, tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`ref`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `ref`(s, t) => sig == s && tpe == t && super.equals(obj)
      case _ => false
    }
  }

  // TODO
  // final case class `const`[T](tpe: Schema[_], value: Value) extends Schema[T] { override def jsonType: String = tpe.jsonType }

  @implicitNotFound("Implicit not found: ValidationBound[${F}, ${T}]. Some of validations doesn't match schema type")
  trait ValidationBound[F, T] {
    def append(
      seq: collection.Seq[ValidationDef[_, _]],
      item: ValidationDef[T, _]): collection.Seq[ValidationDef[_, _]] = seq :+ item
  }
  object ValidationBound {
    def mk[A, B]: ValidationBound[A, B] = new ValidationBound[A, B] {}

    implicit def identity[X]: ValidationBound[X, X] = mk[X, X]

    implicit def numeric[X: Numeric]: ValidationBound[X, Number] = mk[X, Number]

    implicit def intMap[X]: ValidationBound[Map[Int, X], Map[Int, _]] = mk[Map[Int, X], Map[Int, _]]
    implicit def stringMap[X]: ValidationBound[Map[String, X], Map[String, _]] = mk[Map[String, X], Map[String, _]]
    implicit def map[K, V]: ValidationBound[Map[K, V], Map[_, _]] = mk[Map[K, V], Map[_, _]]

    implicit def array[X]: ValidationBound[Array[X], Iterable[_]] = mk[Array[X], Iterable[_]]
    implicit def iterable[X]: ValidationBound[Iterable[X], Iterable[_]] = mk[Iterable[X], Iterable[_]]
//    implicit def seq[X]: ValidationBound[Seq[X], Iterable[_]] = mk[Seq[X], Iterable[_]]
    implicit def list[X]: ValidationBound[List[X], Iterable[_]] = mk[List[X], Iterable[_]]
    implicit def vector[X]: ValidationBound[Vector[X], Iterable[_]] = mk[Vector[X], Iterable[_]]
    implicit def set[X]: ValidationBound[Set[X], Iterable[_]] = mk[Set[X], Iterable[_]]

    implicit def chr: ValidationBound[String, Character] = mk[String, Character]
  }

  object `string` {

    def apply[T](): `string`[T] = `string`[T](None, None)
    def apply[T](pattern: String): `string`[T] = `string`[T](None, Some(pattern))
    def apply[T](format: Format): `string`[T] = `string`[T](Some(format), None)

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