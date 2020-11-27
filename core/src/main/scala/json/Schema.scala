package json

import com.github.andyglow.json._
import json.schema.{ validation => V }


sealed trait Schema[+T] {
  type Self <: Schema[T]

  private var _title: Option[String] = None
  private var _description: Option[String] = None
  private var _validations: collection.Seq[V.Def[_, _]] = Seq.empty
  def jsonType: String
  def withValidation[TT >: T, B](v: V.Def[B, _], vs: V.Def[B, _]*)(implicit bound: V.Magnet[TT, B]): Self = {
    val copy = this.duplicate()
    copy._validations = (v +: vs).foldLeft(_validations) {
      case (agg, v) => bound.append(agg, v)
    }
    copy
  }
  def description: Option[String] = _description
  def title: Option[String] = _title

  // NOTE: `.toSeq` is required for scala 2.13
  // otherwise we'll see
  // type mismatch;
  //  [error]  found   : Seq[json.ValidationDef[_, _]] (in scala.collection)
  //  [error]  required: Seq[json.ValidationDef[_, _]] (in scala.collection.immutable)
  def validations: Seq[V.Def[_, _]] = _validations.toSeq
  protected object ToString {
    def apply(fn: StringBuilder => Unit): String = {
      val sb = new StringBuilder
      fn(sb)
      writeValidations(sb)
      writeExtra(sb)
      sb.toString
    }
    def writeExtra(sb: StringBuilder): Unit = {
      _description foreach { x => sb.append(" description=`").append(x).append('`') }
      _title foreach { x => sb.append(" title=`").append(x).append('`') }
    }
    def writeValidations(sb: StringBuilder): Unit = {
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
    }
  }
  protected def mkCopy(): Self
  def duplicate(
    description: Option[String] = this._description,
    title: Option[String] = this._title): Self = {

    val copy = mkCopy()
    copy._validations = this._validations
    copy._description = description
    copy._title = title

    copy
  }
  def withExtraFrom(x: Schema[_]): Self = {
    val copy = mkCopy()
    copy._validations = x._validations
    copy._description = x._description
    copy._title = x._title

    copy
  }
  def canEqual(that: Any): Boolean = that.isInstanceOf[Schema[_]] // && getClass == that.getClass
  override def equals(obj: Any): Boolean = obj match {
    case s: Schema[_] =>
      s.canEqual(this) &&
      this.title == s.title &&
      this.description == s.description &&
      // compare collections disregarding order
      this.validations.forall(s.validations.contains) &&
      s.validations.forall(this.validations.contains)

    case _ => false
  }
  def withDescription(x: String): Self = duplicate(description = Some(x))
  def withTitle(x: String): Self = duplicate(title = Some(x))
  def toDefinition[TT >: T](sig: String): Schema.`ref`[TT] = Schema.`ref`(sig, this)
}

object Schema {

  // +------------
  // | Boolean
  // +---------------
  //
  sealed class `boolean` extends Schema[Boolean] {
    type Self = `boolean`
    def mkCopy() = new `boolean`
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`boolean`]
    override def jsonType: String = "boolean"
    override def toString: String = ToString(_ append "boolean")
  }
  final object `boolean` extends `boolean`

  // +------------
  // | Integer
  // +---------------
  //
  sealed class `integer` extends Schema[Int] {
    type Self = `integer`
    def mkCopy() = new `integer`()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`integer`]
    override def jsonType: String = "integer"
    override def toString: String = ToString(_ append "integer")
  }
  final object `integer` extends `integer`

  // +------------
  // | Number
  // +---------------
  //
  final class `number`[T: Numeric] extends Schema[T] {
    type Self = `number`[T]
    def mkCopy() = new `number`[T]
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`number`[_]]
    override def jsonType: String = "number"
    override def toString: String = ToString(_ append "number")
  }
  final object `number` {
    def apply[T: Numeric]: `number`[T] = new `number`[T]
  }

  // +------------
  // | String
  // +---------------
  //
  sealed case class `string`[T](format: Option[`string`.Format]) extends Schema[T] {
    type Self = `string`[T]
    def mkCopy() = new `string`[T](format)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`string`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `string`(f) => format == f && super.equals(obj)
      case _ => false
    }
    override def jsonType: String = "string"
    override def toString: String = ToString { sb =>
      sb append "string"
      format foreach { format =>
        sb append "(format = "
        sb append format
        sb append ")"
      }
    }
  }
  final object `string` extends `string`[String](None) {
    def apply[T]: `string`[T] = new `string`[T](None)
    def apply[T](format: Format): `string`[T] = new `string`[T](Some(format))
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

  // +------------
  // | Array
  // +---------------
  //
  final case class `array`[T, C[_]](
    componentType: Schema[T],
    unique: Boolean = false) extends Schema[C[T]] {
    type Self = `array`[T, C]
    def mkCopy() = new `array`[T, C](componentType, unique)
    override def canEqual(that: Any): Boolean = that match {
      case `array`(_, _) => true
      case _             => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `array`(c, u) => u == unique && componentType == c && super.equals(obj)
      case _ => false
    }
    override def jsonType: String = "array"
    override def toString: String = ToString { sb =>
      sb append "array(component ="
      sb append componentType
      sb append ", unique ="
      sb append unique
      sb append ")"
    }
  }

  // +------------
  // | Dictionary
  // +---------------
  //
  final case class `dictionary`[K, V, C[_, _]](valueType: Schema[V]) extends Schema[C[K, V]] {
    type Self = `dictionary`[K, V, C]
    override def jsonType = "object"
    def mkCopy() = new `dictionary`[K, V, C](valueType)
    override def canEqual(that: Any): Boolean = that match {
      case `dictionary`(_) => true
      case _               => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `dictionary`(c) => valueType == c && super.equals(obj)
      case _               => false
    }
    override def toString: String = ToString { sb =>
      sb append "dictionary(value ="
      sb append valueType
      sb append ")"
    }
  }
  final object `dictionary` {
    abstract class KeyPattern[T](val pattern: String)
    final object KeyPattern {
      def mk[T](pattern: String): KeyPattern[T] = new KeyPattern[T](pattern) {}
      def forEnum[T](vals: Iterable[String]): KeyPattern[T] = {
        require(vals.nonEmpty)
        mk[T](vals.toList.distinct.sorted.mkString("^(?:", "|", ")$"))
      }
      implicit final object StringKeyPattern extends KeyPattern[String]("^.*$")
      implicit final object CharKeyPattern extends KeyPattern[Char]("^.{1}$")
      implicit final object ByteKeyPattern extends KeyPattern[Byte]("^[0-9]+$")
      implicit final object ShortKeyPattern extends KeyPattern[Short]("^[0-9]+$")
      implicit final object IntKeyPattern extends KeyPattern[Int]("^[0-9]+$")
      implicit final object LongKeyPattern extends KeyPattern[Long]("^[0-9]+$")
    }
  }

  // +------------
  // | Object
  // +---------------
  //
  sealed case class `object`[T](
    fields: Set[`object`.Field[_]]) extends Schema[T] {
    import `object`._
    type Self = `object`[T]
    def mkCopy() = copy()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`object`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `object`(f) => fields == f && super.equals(obj)
      case _ => false
    }

    def dropField(pred: Field[_] => Boolean): `object`[T] = copy(fields = this.fields.filterNot(pred))
    def withField(f: Field[_]): `object`[T] = copy(fields = fields + f)
    def withFieldsUpdated(pf: PartialFunction[Field[_], Field[_]]): `object`[T] = copy(
      fields = fields collect {
        case f if pf isDefinedAt f => pf(f)
        case f                     => f
      })
    override def jsonType: String = "object"
    override def toString: String = ToString { sb =>
      sb append "object("
      fields foreach { field =>
        sb append field
        sb append ", "
      }
      sb.setLength(sb.length - 1) // drop last comma
      sb append ")"
    }
  }
  final object `object` {
    sealed trait Free { this: `object`[_] => }
    final object Free {
      def apply[T](): `object`[T] with Free = new `object`[T](Set.empty) with Free
    }
    final case class Field[T](
      name: String,
      tpe: Schema[T],
      required: Boolean,
      default: Option[Value],
      description: Option[String]) {
      def canEqual(that: Any): Boolean = that.isInstanceOf[Field[T]]
      override def equals(that: Any): Boolean = canEqual(that) && {
        val other = that.asInstanceOf[Field[T]]
        this.name == other.name &&
          this.required == other.required &&
          this.tpe      == other.tpe &&
          this.default  == other.default
      }
      override def hashCode: Int = name.hashCode
      override def toString: String = {
        var extra = (required, default) match {
          case (true, None)     => " /R"
          case (false, None)    => ""
          case (true, Some(v))  => s" /R /$v"
          case (false, Some(v)) => s" /$v"
        }
        description foreach { x => extra = extra + s" description=`$x`"}
        s"$name: ${tpe}$extra"
      }
      def withDescription(x: Option[String]): Field[T] = new Field(name, tpe, required, default, x)
    }
    final object Field {
      def apply[T](name: String, tpe: Schema[T]): Field[T] = new Field(name, tpe, required = true, default = None, description = None)
      def apply[T](name: String, tpe: Schema[T], required: Boolean): Field[T] = new Field(name, tpe, required, default = None, description = None)
      def apply[T: ToValue](name: String, tpe: Schema[T], required: Boolean, default: T): Field[T] = new Field(name, tpe, required, Some(ToValue(default)), description = None)
      def fromJson[T](name: String, tpe: Schema[T], required: Boolean, default: Option[Value]): Field[T] = new Field(name, tpe, required, default, description = None)
    }
    def apply[T](field: Field[_], xs: Field[_]*): `object`[T] = new `object`((field +: xs.toSeq).toSet)
  }

  // +------------
  // | Enum
  // +---------------
  //
  final case class `enum`[T](
    values: Set[Value]) extends Schema[T] {
    type Self = `enum`[T]
    def mkCopy() = new `enum`[T](values)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`enum`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `enum`(v) => values == v && super.equals(obj)
      case _ => false
    }
    override def jsonType: String = "enum"
    override def toString: String = ToString { sb =>
      sb append "enum("
      values foreach { value =>
        sb append value
        sb append ","
      }
      sb.setLength(sb.length - 1) // drop last comma
      sb append ")"
    }
  }
  final object `enum` {
    def of[T](x: Value, xs: Value*): `enum`[T] = new `enum`[T]((x +: xs).toSet)
  }

  // +------------
  // | OneOf
  // +---------------
  //
  final case class `oneof`[T](
    subTypes: Set[Schema[_]]) extends Schema[T] {
    type Self = `oneof`[T]
    def mkCopy() = new `oneof`[T](subTypes)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`oneof`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `oneof`(s) => subTypes == s && super.equals(obj)
      case _ => false
    }
    override def jsonType: String = "oneof"
    override def toString: String = ToString { sb =>
      sb append "oneof("
      subTypes foreach { tpe =>
        sb append tpe
        sb append ","
      }
      sb.setLength(sb.length - 1) // drop last comma
      sb append ")"
    }
  }
  final object `oneof` {
    def of[T](x: Schema[_], xs: Schema[_]*): `oneof`[T] = new `oneof`[T]((x +: xs).toSet)
  }

  // +------------
  // | AllOf
  // +---------------
  //
  final case class `allof`[T](
    subTypes: Set[Schema[_]]) extends Schema[T] {
    type Self = `allof`[T]
    override def jsonType: String = "allof"
    def mkCopy() = new `allof`[T](subTypes)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`allof`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `allof`(s) => subTypes == s && super.equals(obj)
      case _ => false
    }
    override def toString: String = ToString { sb =>
      sb append "allof("
      subTypes foreach { tpe =>
        sb append tpe
        sb append ","
      }
      sb.setLength(sb.length - 1) // drop last comma
      sb append ")"
    }
  }
  final object `allof` {
    def of[T](x: Schema[_], xs: Schema[_]*): `allof`[T] = new `allof`[T]((x +: xs).toSet)
  }

  // +------------
  // | Not
  // +---------------
  //
  final case class `not`[T](
    tpe: Schema[T]) extends Schema[T] {
    type Self = `not`[T]
    override def jsonType: String = "not"
    def mkCopy() = new `not`[T](tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`not`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `not`(t) => tpe == t && super.equals(obj)
      case _ => false
    }
    override def toString: String = ToString { sb =>
      sb append "not("
      sb append tpe
      sb append ")"
    }
  }

  // +------------
  // | Ref
  // +---------------
  //
  final case class `ref`[T](
    sig: String,
    tpe: Schema[_]) extends Schema[T] {
    type Self = `ref`[T]
    override def jsonType: String = s"$$ref"
    def mkCopy() = new `ref`[T](sig, tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`ref`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `ref`(s, t) => sig == s && tpe == t && super.equals(obj)
      case _ => false
    }
    override def toString: String = ToString { sb =>
      sb append "ref(signature = "
      sb append sig
      sb append ", schema = "
      sb append tpe
      sb append ")"
    }
    override def toDefinition[TT >: T](sig: String): `ref`[TT] = {
      def deepCopy(x: Schema[_]): Schema[_] = {
        val y = x match {
          case `object`(fields)          => new `object`(fields.map { f => f.copy(tpe = deepCopy(f.tpe)) })
          case `array`(y, u)             => `array`(deepCopy(y), u)
          case `dictionary`(y)           => `dictionary`(deepCopy(y))
          case `oneof`(ys)               => `oneof`(ys map deepCopy)
          case `allof`(ys)               => `allof`(ys map deepCopy)
          case `not`(y)                  => `not`(deepCopy(y))
          case `ref`(s, y)               => `ref`(s, deepCopy(y))
          case `lazy-ref`(s) if s == sig => `lazy-ref`(sig)
          case y                         => y
        }
        y withExtraFrom x
      }

      copy(sig = sig, tpe = deepCopy(tpe))
    }
  }

  final object `ref` {
    def apply[T](tpe: Schema[_])(sig: => String): `ref`[T] = tpe match {
      case `ref`(originalSig, innerTpe) => `ref`(originalSig, innerTpe)
      case `value-class`(innerTpe)      => `ref`(sig, innerTpe)
      case _                            => `ref`(sig, tpe)
    }
  }

  // +------------
  // | Value-Class
  // +---------------
  // Pseudo member, doesn't have it's own representation in resulted schema
  //
  final case class `value-class`[O, I](tpe: Schema[I]) extends Schema[O] {
    type Self = `value-class`[O, I]
    override def jsonType: String = ??? // should never call this
    def mkCopy() = new `value-class`[O, I](tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`value-class`[_, _]]
    override def equals(obj: Any): Boolean = obj match {
      case `value-class`(t) => tpe == t && super.equals(obj)
      case _ => false
    }
    override def toString: String = ToString { sb =>
      sb append "value-class("
      sb append tpe
      sb append ")"
    }
    override def toDefinition[TT >: O](sig: String): `ref`[TT] = `ref`[TT](sig, tpe)
  }

  // +------------
  // | LazyRef
  // +---------------
  // Pseudo member, doesn't have it's own representation in resulted schema
  //
  final case class `lazy-ref`[T](sig: String) extends Schema[T] {
    override type Self = `lazy-ref`[T]
    override protected def mkCopy(): `lazy-ref`[T] = `lazy-ref`(sig)
    override def jsonType: String = ??? // should never call this. instead calling code should interpret it as `ref`
    override def toString: String = ToString { sb =>
      sb append "lazy-ref("
      sb append sig
      sb append ")"
    }
  }

  // TODO
  // final case class `const`[T](tpe: Schema[_], value: Value) extends Schema[T] { override def jsonType: String = tpe.jsonType }
}