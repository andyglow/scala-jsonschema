package json

import com.github.andyglow.json.Value.ValueAdapter
import com.github.andyglow.json._
import com.github.andyglow.jsonschema.AsTree
import json.Schema.`object`.Field.RWMode
import json.schema.{validation => V}

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
  def canEqual(that: Any): Boolean       = that.isInstanceOf[Schema[_]] // && getClass == that.getClass
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

  /** This schema shaped by `profile` now (see [[json.schema.JsonSchemaProfile]]): its `option` nodes become plain JSON Schema. Idempotent, so a later render, under any profile, keeps this shape. */
  def withProfile(profile: json.schema.JsonSchemaProfile): Schema[T]                              = profile.lower(this)
  @deprecated("please use `toDefinition` instead", "1.0.0") def apply(refName: String): Schema[T] =
    toDefinition(refName)
}

object Schema extends SchemaCompanionPlatform {

  def apply[T: Schema]: Schema[T] = implicitly

  // +------------
  // | Boolean
  // +---------------
  //
  sealed class `boolean` extends Schema[Boolean] {
    type Self = `boolean`
    def mkCopy()                              = new `boolean`
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`boolean`]
    override def hashCode: Int                = "boolean".hashCode
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
    override def hashCode: Int                = "integer".hashCode
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
    override def hashCode: Int                = "number".hashCode
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
    override def equals(obj: Any): Boolean    = obj match {
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
    def mkCopy()                              = new `array`[T, C](componentType, unique)
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
    override def jsonType                     = "object"
    def mkCopy()                              = new `dictionary`[K, V, C](valueType)
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
      def mk[T](pattern: String): KeyPattern[T]             = new KeyPattern[T](pattern) {}
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
    def mkCopy() = rebuild(fields)

    /** Every copy goes through here (duplicate, withTitle, withField, toDefinition, ...): a copy of a free object stays free. */
    private[json] def rebuild(fs: List[Field[_]]): `object`[T] =
      if (isFree) `object`.Free.of[T](fs) else new `object`[T](fs)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`object`[_]]
    override def equals(obj: Any): Boolean    = obj match {
      case `object`(f) => fields.toSet == f.toSet && isFree == obj.isInstanceOf[Free] && super.equals(obj)
      case _           => false
    }
    // consistent with `equals`: field order doesn't matter
    override def hashCode: Int  = fields.toSet[Field[_]].hashCode
    private def isFree: Boolean = this.isInstanceOf[Free]

    def dropField(pred: Field[_] => Boolean): `object`[T] =
      rebuild(this.fields.filterNot(pred)).withExtraFrom(this)
    def withField(f: Field[_]): `object`[T]                                            = rebuild(fields :+ f).withExtraFrom(this)
    def withField(name: String, tpe: Schema[_], required: Boolean = true): `object`[T] = withField(Field(name, tpe, required))
    def withFieldsUpdated(pf: PartialFunction[Field[_], Field[_]]): `object`[T]        = rebuild(
      fields collect {
        case f if pf isDefinedAt f => pf(f)
        case x                     => x
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
      def apply[T](): `object`[T] with Free = of[T](List.empty)

      private[Schema] def of[T](fs: List[Field[_]]): `object`[T] with Free = {
        new `object`[T](fs) with Free {
          type Type = T
          override def strict: `object`[T] = new `object`[T](this.fields).withExtraFrom(this)
        }
      }
    }
    final case class Field[T](
      name: String,
      tpe: Schema[T],
      required: Boolean,
      default: Option[Value],
      description: Option[String],
      rwMode: Field.RWMode,
      hasDefault: Boolean
    ) {

      /** A field without the [[hasDefault]] fact (the 0.8 constructor). */
      def this(name: String, tpe: Schema[T], required: Boolean, default: Option[Value], description: Option[String], rwMode: Field.RWMode) =
        this(name, tpe, required, default, description, rwMode, default.isDefined)

      def canEqual(that: Any): Boolean        = that.isInstanceOf[Field[_]]
      override def equals(that: Any): Boolean = canEqual(that) && {
        val other = that.asInstanceOf[Field[_]]
        this.name == other.name &&
        this.required == other.required &&
        this.tpe == other.tpe &&
        this.default == other.default &&
        this.description == other.description &&
        this.rwMode == other.rwMode &&
        this.hasDefault == other.hasDefault
      }
      override def hashCode: Int    = name.hashCode
      override def toString: String = {
        var extra = (required, default) match {
          case (true, None)     => " /R"
          case (false, None)    => ""
          case (true, Some(v))  => s" /R /$v"
          case (false, Some(v)) => s" /$v"
        }
        if (hasDefault && default.isEmpty) extra = extra + " /D"
        description foreach { x => extra = extra + s" description=`$x`" }
        s"$name: $tpe: $rwMode$extra"
      }
      def withDescription(x: Option[String]): Field[T] =
        new Field(name, tpe, required, default, x, rwMode, hasDefault)
      def withRWMode(x: RWMode): Field[T] = new Field(name, tpe, required, default, description, x, hasDefault)

      /** Whether the Scala field has a default value. Usually `default` holds it, but not always: `= None` (an `Option` field) and defaults the bridge has no `ToValue` for have no JSON default. A
        * profile uses it where a library omits a field that equals its default (upickle). Not rendered.
        */
      def withHasDefault(x: Boolean): Field[T] = new Field(name, tpe, required, default, description, rwMode, x)
      def setReadOnly: Field[T]                = withRWMode(RWMode.ReadOnly)
      def setWriteOnly: Field[T]               = withRWMode(RWMode.WriteOnly)
    }
    object Field {
      sealed trait RWMode
      object RWMode {
        case object ReadOnly  extends RWMode
        case object WriteOnly extends RWMode
        case object ReadWrite extends RWMode
      }

      /** The 0.8 factory: [[Field.hasDefault]] is `default.isDefined`. */
      def apply[T](name: String, tpe: Schema[T], required: Boolean, default: Option[Value], description: Option[String], rwMode: RWMode): Field[T] =
        new Field(name, tpe, required, default, description, rwMode, default.isDefined)

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
        new Field(name, tpe, required, Some(ToValue(default)), description = None, rwMode = rwMode, hasDefault = true)

      def fromJson[T](
        name: String,
        tpe: Schema[T],
        required: Boolean,
        default: Option[Value],
        rwMode: RWMode = RWMode.ReadWrite
      ): Field[T] = new Field(name, tpe, required, default, description = None, rwMode = rwMode, hasDefault = default.isDefined)
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
    override def equals(obj: Any): Boolean    = obj match {
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
    def of[T](tpe: Schema[_], x: Value, xs: Value*): `enum`[T]                              = new `enum`[T](tpe, (x +: xs).toSet)
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
    override def equals(obj: Any): Boolean    = obj match {
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
    override def equals(obj: Any): Boolean    = obj match {
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
    override def equals(obj: Any): Boolean    = obj match {
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
    override def equals(obj: Any): Boolean    = obj match {
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

    /** Renames this definition: refs to the old name follow it, nested definitions of other types keep their names. */
    override def toDefinition[TT >: T](newSig: String): `def`[TT] = {
      val oldSig                            = this.sig
      def deepCopy(x: Schema[_]): Schema[_] = {
        val y = x match {
          case o: `object`[_]          => o.rebuild(o.fields.map { f => f.copy(tpe = deepCopy(f.tpe)) })
          case `array`(y, u)           => `array`(deepCopy(y), u)
          case `dictionary`(y)         => `dictionary`(deepCopy(y))
          case `oneof`(ys, df)         => `oneof`(ys map deepCopy, df)
          case `allof`(ys)             => `allof`(ys map deepCopy)
          case `not`(y)                => `not`(deepCopy(y))
          case `value-class`(y)        => `value-class`[Any, Any](deepCopy(y).asInstanceOf[Schema[Any]])
          case `option`(y)             => `option`[Any](deepCopy(y).asInstanceOf[Schema[Any]])
          case f: `family`[_]          => f.withMemberSchemas(deepCopy)
          case `def`(s, y)             => `def`(if (s == oldSig) newSig else s, deepCopy(y))
          case `ref`(s) if s == oldSig => `ref`(newSig)
          case y                       => y
        }
        y withExtraFrom x
      }

      copy(sig = newSig, tpe = deepCopy(tpe)).withExtraFrom(this)
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
    override def equals(obj: Any): Boolean    = obj match {
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
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`ref`[_]]
    override def equals(obj: Any): Boolean    = obj match {
      case `ref`(s) => sig == s && super.equals(obj)
      case _        => false
    }
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
    override def toString: String               = ToString { sb =>
      sb append "const("
      sb append JsonFormatter.format(value)
      sb append ": "
      sb append value.tpe
      sb append ")"
    }
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`const`[_]]
    override def equals(obj: Any): Boolean    = obj match {
      case `const`(v) => value == v && super.equals(obj)
      case _          => false
    }
  }

  // +------------
  // | Null
  // +---------------
  // `{"type":"null"}`, valid in every draft from 04 on. The derivation never emits it; a profile's `Option` lowering does (`oneof{X, null}`).
  //
  sealed class `null` extends Schema[Null] {
    type Self = `null`
    def mkCopy()                              = new `null`
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`null`]
    override def hashCode: Int                = "null".hashCode
    override def jsonType: String             = "null"
    override def toString: String             = ToString(_ append "null")
  }
  object `null` extends `null`

  // +------------
  // | Option
  // +---------------
  // "The Scala type here is `Option[T]`", in any position (root, collection element, map value, field). The macros emit it for every `Option`;
  // nesting is kept (`Option[Option[T]]` is option(option(T))). It has no JSON Schema form of its own: a [[json.schema.JsonSchemaProfile]] lowers it
  // before rendering (the default, Neutral, to "T or null"; Legacy to the 0.7 shapes). See `Schema#withProfile`.
  //
  final case class `option`[T](inner: Schema[T]) extends Schema[Option[T]] {
    type Self = `option`[T]
    def mkCopy()                              = new `option`[T](inner)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`option`[_]]
    override def equals(obj: Any): Boolean    = obj match {
      case `option`(i) => inner == i && super.equals(obj)
      case _           => false
    }
    override def jsonType: String = inner.jsonType
    override def toString: String = ToString { sb =>
      sb append "option("
      sb append inner
      sb append ")"
    }
  }

  // +------------
  // | Family
  // +---------------
  // "The Scala type here is a sealed family" (a sealed trait / abstract class of case classes and objects, or a Scala 3 `enum`), with the facts a JSON library's encoding depends on:
  // its members (nested sealed families flattened, in declaration order), their names, kinds, schemas and annotations. Both macros emit it for every sealed family. Like `option`,
  // it has no JSON Schema form of its own: a [[json.schema.JsonSchemaProfile]] lowers it before rendering (Neutral and Legacy to the 0.7 shapes: an untagged `oneOf`, case objects
  // as an `enum`; a library profile to what that library writes). `Either`, `scala.Enumeration` and Java enums are not families.
  //
  final case class `family`[T](members: List[`family`.Member], discriminator: Option[`family`.Discriminator] = None, enumsAsOneOf: Boolean = false) extends Schema[T] {
    type Self = `family`[T]
    def mkCopy()                              = new `family`[T](members, discriminator, enumsAsOneOf)
    override def canEqual(that: Any): Boolean = that.isInstanceOf[`family`[_]]
    override def equals(obj: Any): Boolean    = obj match {
      case `family`(m, d, e) => members == m && discriminator == d && enumsAsOneOf == e && super.equals(obj)
      case _                 => false
    }
    override def hashCode: Int = members.hashCode

    /** The JSON type of what the default profile makes of it. */
    override def jsonType: String = json.schema.JsonSchemaProfile.Neutral.lower(this).jsonType
    override def toString: String = ToString { sb =>
      sb append "family("
      sb append members.mkString(", ")
      discriminator foreach { d => sb append " | " append d }
      if (enumsAsOneOf) sb append " | enumsAsOneOf"
      sb append ")"
    }

    /** The same family with `f` applied to every member's schema. */
    def withMemberSchemas(f: Schema[_] => Schema[_]): `family`[T] = new `family`[T](members.map(m => m.copy(schema = f(m.schema))), discriminator, enumsAsOneOf).withExtraFrom(this)
  }
  object `family` {

    /** One member of a family.
      *
      * @param name
      *   the simple name: `Dog`
      * @param fullName
      *   the class's full name without type arguments: `petshop.Dog` (what play-json's `typeNaming` is given)
      * @param sig
      *   the signature (`Json.sig`): 0.7's default `@discriminator` key and `@definition` name
      * @param kind
      *   what the member is
      * @param schema
      *   for every kind but [[Kind.Singleton]], the member's own schema, without the member's `title`/`description`/`discriminatorKey`/`definition` (the profile applies them). For a singleton, the
      *   schema of its 0.7 enum value ([[value]]): `string`, or what its `@typeHint` resolves to
      * @param value
      *   a singleton's 0.7 enum value: `ToValue[Root](member)` if an implicit `ToValue` of the family was in scope where the schema was derived, otherwise the name. `None` for other kinds
      * @param cls
      *   the member's runtime class (`classOf[Dog]`, `Fish.getClass`), for libraries that key by class (json4s type hints). `None` where the runtime class doesn't identify the member (a Scala 3 enum
      *   case)
      * @param title
      *   the member's own `@title` / scaladoc
      * @param description
      *   the member's own `@description` / scaladoc
      * @param discriminatorKey
      *   `@discriminatorKey`
      * @param definition
      *   `@definition` / `@definition("name")`
      * @param valueField
      *   for a [[Kind.ValueClass]], the name of its one field: the libraries' derived sum codecs write such a member as `{"<field>": value}`
      */
    final case class Member(
      name: String,
      fullName: String,
      sig: String,
      kind: Kind,
      schema: Schema[_],
      value: Option[Value] = None,
      cls: Option[Class[_]] = None,
      title: Option[String] = None,
      description: Option[String] = None,
      discriminatorKey: Option[String] = None,
      definition: Option[Definition] = None,
      valueField: Option[String] = None
    ) {
      // `cls` left out: a Class hashes by identity, which differs per run, and a Set of 5+ members would iterate (render) in a different order
      override def hashCode: Int    = (name, fullName, sig, kind, schema, value, title, description, discriminatorKey, definition, valueField).hashCode
      override def toString: String = {
        val sb = new StringBuilder
        sb append name append ':' append kind append '(' append schema
        value foreach { v => sb append " = " append JsonFormatter.format(v) }
        sb append ')'
        title foreach { x => sb append " title=`" append x append '`' }
        description foreach { x => sb append " description=`" append x append '`' }
        discriminatorKey foreach { x => sb append " discriminatorKey=`" append x append '`' }
        definition foreach { x => sb append " " append x }
        sb.toString
      }
    }

    sealed trait Kind
    object Kind {

      /** A case class, or a Scala 3 `enum` case with parameters. */
      case object Product extends Kind

      /** A value class (`extends AnyVal`). Unwrapped under profiles that honor the schema annotations (Neutral, Legacy, spray: as 0.7); written as the case class `{"<field>": value}` under the
        * library profiles, as their derived sum codecs do.
        */
      case object ValueClass extends Kind

      /** A member whose schema came from an implicit `Schema` or `Predef` in scope: used as it is (0.7 adds none of the member's annotations to it). */
      case object Provided extends Kind

      /** A case object, or a Scala 3 `enum` case without parameters. */
      case object Singleton extends Kind
    }

    /** `@definition` on a member: [[Definition.Inferred]] (no argument: named by the signature) or [[Definition.Named]]. */
    sealed trait Definition
    object Definition {
      case object Inferred                 extends Definition
      final case class Named(name: String) extends Definition
    }

    /** The value schema of a singleton whose value comes from an implicit `ToValue` of the family without a `@typeHint` (what the codec writes, known only at runtime): `string` for a string,
      * otherwise the value itself as a `const` (Neutral and Legacy keep such a member a `const`).
      */
    def valueSchema(v: Value): Schema[_] = v match {
      case _: Value.str => `string`[String](None)
      case other        => `const`[Any](other)
    }

    /** `@discriminator` on the family, its defaults resolved. */
    final case class Discriminator(field: String = "type", phantom: Boolean = true)
  }
}
