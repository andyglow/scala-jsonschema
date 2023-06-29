package json

import com.github.andyglow.json.Value.ValueAdapter
import com.github.andyglow.json._
import com.github.andyglow.jsonschema.AsTree
import json.Schema.`object`.Field.RWMode
import json.schema.{validation => V}

import scala.collection.immutable.ListMap

sealed trait Schema[+T] {
  type Self <: Schema[T]

  private var _title: Option[String]                    = None
  private var _description: Option[String]              = None
  private var _discriminationKey: Option[String]        = None
  private var _validations: collection.Seq[V.Def[_, _]] = Seq.empty
  def jsonType: String
  def withValidation[TT >: T, B](v: V.Def[B, _], vs: V.Def[B, _]*)(implicit
    bound: V.Magnet[TT, B]
  ): Self = {
    val copy = this.duplicate()
    copy._validations = (v +: vs).foldLeft(_validations) { case (agg, v) =>
      bound.append(agg, v)
    }
    copy
  }
  def description: Option[String]       = _description
  def title: Option[String]             = _title
  def discriminationKey: Option[String] = _discriminationKey

  // NOTE: `.toSeq` is required for scala 2.13
  // otherwise we'll see
  // type mismatch;
  //  [error]  found   : Seq[json.ValidationDef[_, _]] (in scala.collection)
  //  [error]  required: Seq[json.ValidationDef[_, _]] (in scala.collection.immutable)
  def validations: Seq[V.Def[_, _]] = _validations.toSeq
  def toDebugString: String         = AsTree(this).rendered
  protected object ToString {
    def apply(fn: StringBuilder => Any): String = {
      val sb = new StringBuilder
      fn(sb)
      writeValidations(sb)
      writeExtra(sb)
      sb.toString
    }
    def writeExtra(sb: StringBuilder): Unit = {
      _description foreach { x => sb.append(" description=`").append(x).append('`') }
      _title foreach { x => sb.append(" title=`").append(x).append('`') }
      _discriminationKey foreach { x => sb.append(" discriminationKey=`").append(x).append('`') }
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

      ()
    }
  }
  protected def mkCopy(): Self
  def duplicate(
    description: Option[String] = this._description,
    title: Option[String] = this._title,
    discriminationKey: Option[String] = this._discriminationKey
  ): Self = {

    val copy = mkCopy()
    copy._validations = this._validations
    copy._discriminationKey = discriminationKey
    copy._description = description
    copy._title = title

    copy
  }
  def withExtraFrom(x: Schema[_]): Self = {
    val copy = mkCopy()
    copy._validations = x._validations
    copy._description = x._description
    copy._title = x._title
    copy._discriminationKey = x._discriminationKey

    copy
  }
  def withValidationsAddedFrom(x: Schema[_]): Self = {
    val copy = mkCopy()
    copy._validations = copy._validations ++ x._validations

    copy
  }
  def canEqual(that: Any): Boolean = that.isInstanceOf[Schema[_]] // && getClass == that.getClass
  override def equals(obj: Any): Boolean = obj match {
    case s: Schema[_] =>
      s.canEqual(this) &&
      this.title == s.title &&
      this.description == s.description &&
      this.discriminationKey == s.discriminationKey &&
      // compare collections disregarding order
      this.validations.forall(s.validations.contains) &&
      s.validations.forall(this.validations.contains)

    case _ => false
  }
  def withDescription(x: String): Self                     = duplicate(description = Some(x))
  def withTitle(x: String): Self                           = duplicate(title = Some(x))
  def withDiscriminationKey(x: String): Self               = duplicate(discriminationKey = Some(x))
  def toDefinition[TT >: T](sig: String): Schema.`def`[TT] = Schema.`def`(sig, this)
  @deprecated("please use `toDefinition` instead", "1.0.0") def apply(refName: String): Schema[T] =
    toDefinition(refName)
}

object Schema {

  def apply[T: Schema]: Schema[T] = implicitly

  // +------------
  // | Boolean
  // +---------------
  //
  sealed class `boolean` extends Schema[Boolean] {
    type Self = `boolean`
    def mkCopy()                              = new `boolean`
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`boolean`]
    override def jsonType: String             = "boolean"
    override def toString: String             = ToString(_ append "boolean")
  }
  object `boolean` extends `boolean`

  // +------------
  // | Integer
  // +---------------
  //
  sealed class `integer` extends Schema[Int] {
    type Self = `integer`
    def mkCopy()                              = new `integer`()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`integer`]
    override def jsonType: String             = "integer"
    override def toString: String             = ToString(_ append "integer")
  }
  object `integer` extends `integer`

  // +------------
  // | Number
  // +---------------
  //
  final class `number`[T: Numeric] extends Schema[T] {
    type Self = `number`[T]
    def mkCopy()                              = new `number`[T]
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`number`[_]]
    override def jsonType: String             = "number"
    override def toString: String             = ToString(_ append "number")
  }
  object `number` {
    def apply[T: Numeric]: `number`[T] = new `number`[T]
  }

  // +------------
  // | String
  // +---------------
  //
  sealed case class `string`[T](format: Option[`string`.Format]) extends Schema[T] {
    type Self = `string`[T]
    def mkCopy()                              = new `string`[T](format)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`string`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `string`(f) => format == f && super.equals(obj)
      case _           => false
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
  object `string` extends `string`[String](None) {
    def apply[T]: `string`[T]                 = new `string`[T](None)
    def apply[T](format: Format): `string`[T] = new `string`[T](Some(format))
    trait Format extends Product
    object Format {
      case object `date`      extends Format
      case object `time`      extends Format
      case object `date-time` extends Format // Date representation, as defined by RFC 3339, section 5.6.
      case object `email`     extends Format // Internet email address, see RFC 5322, section 3.4.1.
      case object `hostname`  extends Format // Internet host name, see RFC 1034, section 3.1.
      case object `ipv4`      extends Format // Internet host name, see RFC 1034, section 3.1.
      case object `ipv6`      extends Format // IPv6 address, as defined in RFC 2373, section 2.2.
      case object `uri`       extends Format // A universal resource identifier (URI), according to RFC3986.

      // added in 2019-09
      case object `duration`     extends Format // The duration format is from the ISO 8601 ABNF as given in Appendix A of RFC 3339
      case object `idn-hostname` extends Format // Use RFC 1123 instead of RFC 1034; this allows for a leading digit,
      // `hostname` is also RFC 1123 since 2019-09
      case object `uuid` extends Format // A string instance is valid against this attribute if it is a valid string representation of a UUID, according to RFC4122
    }
  }

  // +------------
  // | Array
  // +---------------
  //
  final case class `array`[T, C[_]](componentType: Schema[T], unique: Boolean = false) extends Schema[C[T]] {
    type Self = `array`[T, C]
    def mkCopy() = new `array`[T, C](componentType, unique)
    override def canEqual(that: Any): Boolean = that match {
      case `array`(_, _) => true
      case _             => false
    }
    override def equals(obj: Any): Boolean = obj match {
      case `array`(c, u) => u == unique && componentType == c && super.equals(obj)
      case _             => false
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
    def mkCopy()          = new `dictionary`[K, V, C](valueType)
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
  object `dictionary` {
    abstract class KeyPattern[T](val pattern: String)
    object KeyPattern {
      def mk[T](pattern: String): KeyPattern[T] = new KeyPattern[T](pattern) {}
      def forEnum[T](vals: Iterable[String]): KeyPattern[T] = {
        require(vals.nonEmpty)
        mk[T](vals.toList.distinct.sorted.mkString("^(?:", "|", ")$"))
      }
      implicit object StringKeyPattern extends KeyPattern[String]("^.*$")
      implicit object CharKeyPattern   extends KeyPattern[Char]("^.{1}$")
      implicit object ByteKeyPattern   extends KeyPattern[Byte]("^[0-9]+$")
      implicit object ShortKeyPattern  extends KeyPattern[Short]("^[0-9]+$")
      implicit object IntKeyPattern    extends KeyPattern[Int]("^[0-9]+$")
      implicit object LongKeyPattern   extends KeyPattern[Long]("^[0-9]+$")
    }
  }

  // +------------
  // | Object
  // +---------------
  //
  sealed case class `object`[T] private (fields: List[`object`.Field[_]]) extends Schema[T] {
    import `object`._
    type Self = `object`[T]
    def mkCopy()                              = copy()
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`object`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `object`(f) => fields.toSet == f.toSet && super.equals(obj)
      case _           => false
    }

    def dropField(pred: Field[_] => Boolean): `object`[T] =
      copy(fields = this.fields.filterNot(pred)).withExtraFrom(this)
    def withField(f: Field[_]): `object`[T]                                            = copy(fields = fields :+ f).withExtraFrom(this)
    def withField(name: String, tpe: Schema[_], required: Boolean = true): `object`[T] = withField(Field(name, tpe, required))
    def withFieldsUpdated(pf: PartialFunction[Field[_], Field[_]]): `object`[T] = copy(
      fields = fields collect {
        case f if pf isDefinedAt f => pf(f)
        case x                          => x
      }
    ).withExtraFrom(this)
    override def jsonType: String = "object"
    override def toString: String = ToString { sb =>
      sb append "object("
      fields foreach { field =>
        sb append field
        sb append ", "
      }
      sb.setLength(sb.length - 1) // drop last comma
      sb append ")"

      if (this.isInstanceOf[Free]) { sb append " additionalProperties=true" }
    }

    def free: `object`[T] with Free = {
      val self = this
      new `object`[T](fields) with Free {
        type Type = T
        def strict: `object`[T] = self
      }
    }
  }
  object `object` {
    sealed trait Free { this: `object`[_] =>
      type Type
      def strict: `object`[Type]
    }
    object Free {
      def apply[T](): `object`[T] with Free = {
        new `object`[T](List.empty) with Free {
          type Type = T
          override def strict: `object`[T] = new `object`[T](List.empty)
        }
      }
    }
    final case class Field[T](
      name: String,
      tpe: Schema[T],
      required: Boolean,
      default: Option[Value],
      description: Option[String],
      rwMode: Field.RWMode
    ) {
      def canEqual(that: Any): Boolean = that.isInstanceOf[Field[T]]
      override def equals(that: Any): Boolean = canEqual(that) && {
        val other = that.asInstanceOf[Field[T]]
        this.name == other.name &&
        this.required == other.required &&
        this.tpe == other.tpe &&
        this.default == other.default &&
        this.rwMode == other.rwMode
      }
      override def hashCode: Int = name.hashCode
      override def toString: String = {
        var extra = (required, default) match {
          case (true, None)     => " /R"
          case (false, None)    => ""
          case (true, Some(v))  => s" /R /$v"
          case (false, Some(v)) => s" /$v"
        }
        description foreach { x => extra = extra + s" description=`$x`" }
        s"$name: $tpe: $rwMode$extra"
      }
      def withDescription(x: Option[String]): Field[T] =
        new Field(name, tpe, required, default, x, rwMode)
      def withRWMode(x: RWMode): Field[T] = new Field(name, tpe, required, default, description, x)
      def setReadOnly: Field[T]           = withRWMode(RWMode.ReadOnly)
      def setWriteOnly: Field[T]          = withRWMode(RWMode.WriteOnly)
    }
    object Field {
      sealed trait RWMode
      object RWMode {
        case object ReadOnly  extends RWMode
        case object WriteOnly extends RWMode
        case object ReadWrite extends RWMode
      }

      def apply[T](name: String, tpe: Schema[T]): Field[T] =
        new Field(name, tpe, required = true, default = None, description = None, RWMode.ReadWrite)

      def apply[T](name: String, tpe: Schema[T], required: Boolean): Field[T] =
        new Field(name, tpe, required, default = None, description = None, RWMode.ReadWrite)

      def apply[T: ToValue](
        name: String,
        tpe: Schema[T],
        required: Boolean,
        default: T,
        rwMode: RWMode = RWMode.ReadWrite
      ): Field[T] =
        new Field(name, tpe, required, Some(ToValue(default)), description = None, rwMode = rwMode)

      def fromJson[T](
        name: String,
        tpe: Schema[T],
        required: Boolean,
        default: Option[Value],
        rwMode: RWMode = RWMode.ReadWrite
      ): Field[T] = new Field(name, tpe, required, default, description = None, rwMode = rwMode)
    }

    def apply[T](field: Field[_], xs: Field[_]*): `object`[T] = {
      fromList(field +: xs.toList)
    }

    def fromList[T](fields: List[Field[_]]): `object`[T] = {
      `object`(fields.distinct)
    }
  }

  // +------------
  // | Enum
  // +---------------
  //
  final case class `enum`[T](tpe: Schema[_], values: Set[Value]) extends Schema[T] {
    type Self = `enum`[T]
    def mkCopy()                              = new `enum`[T](tpe, values)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`enum`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `enum`(t, v) => t == tpe && values == v && super.equals(obj)
      case _            => false
    }
    override def jsonType: String = "enum"
    override def toString: String = ToString { sb =>
      sb append "enum["
      sb append tpe.toString
      sb append "]("
      values foreach { value =>
        sb append value
        sb append ","
      }
      sb.setLength(sb.length - 1) // drop last comma
      sb append ")"
    }
  }
  object `enum` {
    def of[T](tpe: Schema[_], x: Value, xs: Value*): `enum`[T] = new `enum`[T](tpe, (x +: xs).toSet)
    def of[T](x: T, xs: T*)(implicit va: ValueAdapter[T], vs: ValueSchema[T]): `enum`[vs.S] = {
      new `enum`[vs.S](vs.schema, (x +: xs).toSet.map { (x: T) => va.adapt(x) })
    }
  }

  // +------------
  // | OneOf
  // +---------------
  //
  final case class `oneof`[T](subTypes: Set[Schema[_]], discriminationField: Option[String] = None) extends Schema[T] {
    type Self = `oneof`[T]
    def mkCopy()                              = new `oneof`[T](subTypes, discriminationField)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`oneof`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `oneof`(s, d) => subTypes == s && discriminationField == d && super.equals(obj)
      case _             => false
    }
    override def jsonType: String = "oneof"
    override def toString: String = ToString { sb =>
      sb append "oneof("
      subTypes foreach { tpe =>
        sb append tpe
        sb append ","
      }
      sb.setLength(sb.length - 1) // drop last comma
      discriminationField foreach { f =>
        sb append "| discriminationField="
        sb append f
      }
      sb append ")"
    }
    def discriminatedBy(x: String): Self = new `oneof`[T](subTypes, Some(x))
  }
  object `oneof` {
    def of[T](x: Schema[_], xs: Schema[_]*): `oneof`[T] = new `oneof`[T]((x +: xs).toSet, None)
  }

  // +------------
  // | AllOf
  // +---------------
  //
  final case class `allof`[T](subTypes: Set[Schema[_]]) extends Schema[T] {
    type Self = `allof`[T]
    override def jsonType: String             = "allof"
    def mkCopy()                              = new `allof`[T](subTypes)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`allof`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `allof`(s) => subTypes == s && super.equals(obj)
      case _          => false
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
  object `allof` {
    def of[T](x: Schema[_], xs: Schema[_]*): `allof`[T] = new `allof`[T]((x +: xs).toSet)
  }

  // +------------
  // | Not
  // +---------------
  //
  final case class `not`[T](tpe: Schema[T]) extends Schema[T] {
    type Self = `not`[T]
    override def jsonType: String             = "not"
    def mkCopy()                              = new `not`[T](tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`not`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `not`(t) => tpe == t && super.equals(obj)
      case _        => false
    }
    override def toString: String = ToString { sb =>
      sb append "not("
      sb append tpe
      sb append ")"
    }
  }

  // +------------
  // | Def
  // +---------------
  //
  final case class `def`[T](sig: String, tpe: Schema[_]) extends Schema[T] {
    type Self = `def`[T]
    override def jsonType: String             = ??? // s"$$ref"
    def mkCopy()                              = new `def`[T](sig, tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`def`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `def`(s, t) => sig == s && tpe == t && super.equals(obj)
      case _           => false
    }
    override def toString: String = ToString { sb =>
      sb append "def(signature = "
      sb append sig
      sb append ", schema = "
      sb append tpe
      sb append ")"
    }
    override def withValidation[TT >: T, B](v: V.Def[B, _], vs: V.Def[B, _]*)(implicit
      bound: V.Magnet[TT, B]
    ): `def`[T] = copy(tpe = tpe.asInstanceOf[Schema[TT]].withValidation(v, vs: _*)).withExtraFrom(this)
    override def toDefinition[TT >: T](sig: String): `def`[TT] = {
      def deepCopy(x: Schema[_]): Schema[_] = {
        val y = x match {
          case `object`(fields)     => `object`(fields.map { f => f.copy(tpe = deepCopy(f.tpe)) })
          case `array`(y, u)        => `array`(deepCopy(y), u)
          case `dictionary`(y)      => `dictionary`(deepCopy(y))
          case `oneof`(ys, df)      => `oneof`(ys map deepCopy, df)
          case `allof`(ys)          => `allof`(ys map deepCopy)
          case `not`(y)             => `not`(deepCopy(y))
          case `def`(s, y)          => `def`(sig, deepCopy(y))
          case `ref`(s) if s == sig => `ref`(sig)
          case y                    => y
        }
        y withExtraFrom x
      }

      copy(sig = sig, tpe = deepCopy(tpe)).withExtraFrom(this)
    }
  }

  object `def` {
    def adapt[T](tpe: Schema[_], sig: => String): `def`[T] = {
      tpe match {
        case `def`(originalSig, innerTpe) => `def`(originalSig, innerTpe)
        case `value-class`(innerTpe)      => `def`(sig, innerTpe)
        case _                            => `def`(sig, tpe)
      }
    }
  }

  // +------------
  // | Value-Class
  // +---------------
  // Pseudo member, doesn't have it's own representation in resulted schema
  //
  final case class `value-class`[O, I](tpe: Schema[I]) extends Schema[O] {
    type Self = `value-class`[O, I]
    override def jsonType: String             = tpe.jsonType
    def mkCopy()                              = new `value-class`[O, I](tpe)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`value-class`[_, _]]
    override def equals(obj: Any): Boolean = obj match {
      case `value-class`(t) => tpe == t && super.equals(obj)
      case _                => false
    }
    override def toString: String = ToString { sb =>
      sb append "value-class("
      sb append tpe
      sb append ")"
    }
    override def toDefinition[TT >: O](sig: String): `def`[TT] =
      `def`[TT](sig, tpe.withValidationsAddedFrom(this))
  }

  // +------------
  // | LazyRef
  // +---------------
  // Pseudo member, doesn't have it's own representation in resulted schema
  //
  final case class `ref`[T](sig: String) extends Schema[T] {
    override type Self = `ref`[T]
    override protected def mkCopy(): `ref`[T] = `ref`(sig)
    override def jsonType: String =
      ??? // should never call this. instead calling code should interpret it as `ref`
    override def toString: String = ToString { sb =>
      sb append "ref("
      sb append sig
      sb append ")"
    }
  }

  // +------------
  // | Const
  // +---------------
  // It is used in conjunction with OneOf as Alternative to Enum
  // INTERNAL API
  final case class `const`[T](value: Value) extends Schema[T] {
    override def jsonType: String = "const"
    override type Self = `const`[T]
    override protected def mkCopy(): `const`[T] = `const`(value)
    override def toString: String = ToString { sb =>
      sb append "const("
      sb append JsonFormatter.format(value)
      sb append ": "
      sb append value.tpe
      sb append ")"
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`const`[_]]
    override def equals(obj: Any): Boolean = obj match {
      case `const`(v) => value == v && super.equals(obj)
      case _          => false
    }
  }
}
