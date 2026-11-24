package json.schema

import com.github.andyglow.json.Value
import json.Schema
import json.Schema._

/** How the JSON a schema describes is shaped, for the facts the derivation records but doesn't decide.
  *
  * `Json.schema[T]` (and `derives json.Schema`) records facts that a JSON library decides how to encode:
  *   - "this is an `Option`": the `option` node, in every position (root, collection element, map value, field);
  *   - "this is a sealed family": the `family` node, with its members' names, kinds, schemas and annotations.
  *
  * A profile lowers those nodes to plain JSON Schema when the schema is rendered, or earlier with `schema.withProfile(profile)`. Everything else (Predefs, hand-written schemas, all other derived
  * nodes) passes through unchanged.
  *
  * Which profile applies to a render:
  *   1. `schema.withProfile(p)`: shaped already, so the render's profile finds nothing left to lower;
  *   1. otherwise an implicit `JsonSchemaProfile` in scope where the schema is rendered (`AsValue.schema`, `asPlay`, `asCirce`, ..., `stringify`);
  *   1. otherwise [[JsonSchemaProfile.Neutral]] (found in the implicit scope, so any profile in lexical scope wins over it).
  *
  * The bridges provide profiles that describe what their library writes (`PlayProfile`, `CirceProfile`, `UPickleProfile`, `Json4sProfile`, `SprayProfile`, and the
  * `com.github.andyglow.jsonschema.profiles.<lib>._` imports). A profile of your own starts from one of them, or from [[JsonSchemaProfile.Neutral]], and changes knobs with the `with...` builders.
  *
  * {{{
  * implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy   // where you render: the 0.7 output (Scala 3: given JsonSchemaProfile = ...)
  * Json.schema[Foo].withProfile(JsonSchemaProfile.Legacy)                // or per schema
  * }}}
  */
trait JsonSchemaProfile extends JsonSchemaProfileDefault {
  import JsonSchemaProfile._

  /** A name for messages and `toString`. */
  def name: String

  /** How `Option` is shaped. */
  def optionStyle: OptionStyle

  /** Whether the wire-shape annotations of sealed families, `@discriminator` and `@discriminatorKey`, apply. True for [[JsonSchemaProfile.Neutral]], [[JsonSchemaProfile.Legacy]] and spray, where the
    * user writes the sum codec; false for the profiles of libraries that derive it (play, circe, upickle, json4s), whose derivation never sees those annotations.
    *
    * When true:
    *   - a family annotated `@discriminator(field)` is tagged by `field`, whatever [[sumStyle]] says; each member's tag is its `@discriminatorKey`, else its signature (`Json.sig`), as in 0.7;
    *   - an unannotated family is shaped by [[sumStyle]]; a member's `@discriminatorKey`, if any, replaces [[memberName]].
    *
    * When false, both annotations are ignored: [[sumStyle]] and [[memberName]] decide. `@title`, `@description` and `@definition` apply under every profile.
    */
  def honorsSchemaAnnotations: Boolean = true

  /** Under [[OptionStyle.Nullable]]: an `Option` field without a default is required (its key must be present, its value may be `null`). For libraries whose reader rejects a missing `Option` key
    * (upickle). A field with a default (`= None` included) stays optional.
    */
  def optionKeyRequired: Boolean = false

  /** How the members of a sealed family are told apart. */
  def sumStyle: SumStyle = SumStyle.Untagged

  /** How a singleton member (case object, Scala 3 `enum` case without parameters) is written. */
  def singletonStyle: SingletonStyle = SingletonStyle.AsString

  /** The name a member is tagged with ([[SumStyle.Discriminator]], [[SumStyle.WrapperObject]]), or written as ([[SingletonStyle.AsString]] when [[honorsSchemaAnnotations]] is false). */
  def memberName: MemberName = MemberName.FullName

  // builders: a profile with one knob changed and every other knob kept

  final def withName(x: String): JsonSchemaProfile                     = Knobs(this).copy(name = x).profile
  final def withOption(x: OptionStyle): JsonSchemaProfile              = Knobs(this).copy(optionStyle = x).profile
  final def withOptionKeyRequired(x: Boolean): JsonSchemaProfile       = Knobs(this).copy(optionKeyRequired = x).profile
  final def withSum(x: SumStyle): JsonSchemaProfile                    = Knobs(this).copy(sumStyle = x).profile
  final def withSingleton(x: SingletonStyle): JsonSchemaProfile        = Knobs(this).copy(singletonStyle = x).profile
  final def withMemberName(x: MemberName): JsonSchemaProfile           = Knobs(this).copy(memberName = x).profile
  final def withHonorsSchemaAnnotations(x: Boolean): JsonSchemaProfile = Knobs(this).copy(honorsSchemaAnnotations = x).profile

  /** `schema` with every `option` and `family` node lowered to plain JSON Schema nodes. Idempotent: a schema without those nodes comes back equal. */
  final def lower[T](schema: Schema[T]): Schema[T] = new Lowering(this)(schema).asInstanceOf[Schema[T]]

  override def toString: String = s"JsonSchemaProfile($name)"
}

object JsonSchemaProfile {

  sealed trait OptionStyle
  object OptionStyle {

    /** The 0.7 shapes. An `Option` field that is not required is its element's schema (one `Option` layer is dropped); every other `Option[X]` is `oneOf(object{value: X}, enum["None"])`, `Option`
      * derived as the sealed family `Some | None`.
      */
    case object Legacy extends OptionStyle

    /** `X` or `null`, in every position. `Option[Option[X]]` is the same as `Option[X]`. Renders as `"type": [X, "null"]` where `X` has a single JSON type, otherwise as `oneOf[X, {"type": "null"}]`.
      */
    case object Nullable extends OptionStyle
  }

  sealed trait SumStyle
  object SumStyle {

    /** `oneOf` the members as they are, no tag (0.7; json4s without type hints). */
    case object Untagged extends SumStyle

    /** `{"<name>": member}` (circe-generic). */
    case object WrapperObject extends SumStyle

    /** The member's own fields plus `"<field>": "<name>"` (play `_type`, upickle `$type`, circe configured, json4s type hints). A member from an implicit `Schema` that is a definition is inlined,
      * tagged, in the `oneOf` (its definition stays as it is elsewhere). One whose schema isn't an object can't carry the tag: lowering fails with a message naming the member.
      */
    final case class Discriminator(field: String) extends SumStyle

    /** A style chosen for each family from its facts, for libraries whose sum encoding depends on the members (json4s type hints are registered per class: a family whose classes have no hint is
      * written untagged, one whose classes have hints is tagged). `f` may throw to reject a family the library would write in a way no style describes; the exception reaches the caller of the render
      * (or `lower`). Applies where [[Untagged]] etc. would: not to a family whose `@discriminator` is honored. `f` returning a `PerFamily` is an `IllegalArgumentException`. Profiles built with it
      * don't compare equal.
      */
    final case class PerFamily(f: `family`[_] => SumStyle) extends SumStyle
  }

  sealed trait SingletonStyle
  object SingletonStyle {

    /** A string: `"Fish"` (0.7, upickle). Under [[honorsSchemaAnnotations]], the 0.7 value (an implicit `ToValue` of the family where the schema was derived, else the name); otherwise the
      * [[memberName]]. Singletons with equal value schemas merge into one `enum` (or, with `Flag.EnumsAsOneOf`, a `const` each).
      */
    case object AsString extends SingletonStyle

    /** An object without fields, tagged like the other members: `{"Fish": {}}` (circe), `{"_type": "Fish"}` (play), `{}` (json4s without hints). */
    case object AsTagged extends SingletonStyle
  }

  sealed trait MemberName {
    def apply(m: `family`.Member): String
  }
  object MemberName {

    /** `Dog` */
    case object SimpleName extends MemberName { def apply(m: `family`.Member): String = m.name }

    /** `petshop.Dog`: the class's full name, without type arguments */
    case object FullName extends MemberName { def apply(m: `family`.Member): String = m.fullName }

    /** Anything computed from the member's facts: play's `typeNaming(fullName)`, json4s' `hintFor(cls)`. Profiles built with it don't compare equal. */
    final case class Custom(f: `family`.Member => String) extends MemberName { def apply(m: `family`.Member): String = f(m) }
  }

  /** The default: the 0.7 shapes, with every `Option[X]` as "X or null". */
  val Neutral: JsonSchemaProfile = Knobs("neutral", OptionStyle.Nullable).profile

  /** The 0.7 output, byte for byte. */
  val Legacy: JsonSchemaProfile = Knobs("legacy", OptionStyle.Legacy).profile

  /** Every knob of a profile; `profile` builds one from them. */
  private final case class Knobs(
    name: String,
    optionStyle: OptionStyle,
    optionKeyRequired: Boolean = false,
    sumStyle: SumStyle = SumStyle.Untagged,
    singletonStyle: SingletonStyle = SingletonStyle.AsString,
    memberName: MemberName = MemberName.FullName,
    honorsSchemaAnnotations: Boolean = true
  ) { self =>
    def profile: JsonSchemaProfile = new JsonSchemaProfile {
      val name: String                              = self.name
      val optionStyle: OptionStyle                  = self.optionStyle
      override val optionKeyRequired: Boolean       = self.optionKeyRequired
      override val sumStyle: SumStyle               = self.sumStyle
      override val singletonStyle: SingletonStyle   = self.singletonStyle
      override val memberName: MemberName           = self.memberName
      override val honorsSchemaAnnotations: Boolean = self.honorsSchemaAnnotations
    }
  }
  private object Knobs {
    def apply(p: JsonSchemaProfile): Knobs =
      Knobs(p.name, p.optionStyle, p.optionKeyRequired, p.sumStyle, p.singletonStyle, p.memberName, p.honorsSchemaAnnotations)
  }

  private final class Lowering(p: JsonSchemaProfile) {

    def apply(s: Schema[_]): Schema[_] = s match {
      case `option`(inner)  => optionAt(inner).withExtraFrom(s)
      case f: `family`[_]   => familyAt(f).withExtraFrom(s)
      case o: `object`[_]   => o.withFieldsUpdated { case f => field(f) }
      case `array`(c, u)    => `array`[Any, List](apply(c).asInstanceOf[Schema[Any]], u).withExtraFrom(s)
      case `dictionary`(v)  => `dictionary`[Any, Any, Map](apply(v).asInstanceOf[Schema[Any]]).withExtraFrom(s)
      case `oneof`(xs, df)  => `oneof`[Any](xs map apply, df).withExtraFrom(s)
      case `allof`(xs)      => `allof`[Any](xs map apply).withExtraFrom(s)
      case `not`(t)         => `not`[Any](apply(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
      case `def`(sig, t)    => `def`[Any](sig, apply(t)).withExtraFrom(s)
      case `value-class`(t) => `value-class`[Any, Any](apply(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
      case other            => other // leaves, enum, const, ref: no nested schema that could hold an `option` or a `family`
    }

    // ---- Option

    private def field(f: `object`.Field[_]): `object`.Field[_] = f.tpe match {
      // 0.7 dropped one layer for an `Option` field; the macros make every `Option` field not required
      case `option`(inner) if p.optionStyle == OptionStyle.Legacy && !f.required                            => f.copy(tpe = apply(inner))
      case t @ `option`(_) if p.optionStyle == OptionStyle.Nullable && p.optionKeyRequired && !f.hasDefault => f.copy(tpe = apply(t), required = true)
      case t                                                                                                => f.copy(tpe = apply(t))
    }

    /** An `option` node anywhere but in a Legacy `Option` field. */
    private def optionAt(inner: Schema[_]): Schema[_] = p.optionStyle match {
      case OptionStyle.Nullable =>
        val x = apply(flatten(inner))
        // an `any` (or a definition / value class of one) admits `null` already, and `oneOf[{}, null]` would reject it: X alone
        if (Schema.`any`.admitsEverything(x)) x else `oneof`[Any](Set(x, Schema.`null`))
      case OptionStyle.Legacy =>
        // exactly what the Scala 2 macro derived in 0.7, member order included (a 2-element Set iterates in insertion order, and the renderer keeps it)
        `oneof`[Any](
          Set(
            `enum`[Any](`string`[String](None), Set[Value](Value.str("None"))),
            `object`.fromList[Any](List(`object`.Field("value", apply(inner), required = true)))
          ),
          None
        )
    }

    private def flatten(s: Schema[_]): Schema[_] = s match {
      case `option`(inner) => flatten(inner)
      case other           => other
    }

    // ---- sealed families
    //
    // Under Neutral and Legacy this reproduces what the 0.7 macro derived, in the same member order (`oneof` and `enum` hold Sets: built in the same insertion order, they iterate the same way):
    //   - a case class / value class member gets the member's title, description and `@discriminatorKey` where it has none of its own; with `@discriminator`, its key is set to
    //     `@discriminatorKey`, else the signature; then `@definition` wraps it. A member from an implicit Schema/Predef is used as it is;
    //   - singletons with equal value schemas merge into one `enum` (moved to the end at every merge), their texts dropped; with `enumsAsOneOf`, a `const` each, with its texts and key;
    //   - a family of singletons only is that `enum` (or a `oneOf` of the `const`s) without a `oneOf` around it; `@discriminator` doesn't apply to it.

    private def familyAt(f: `family`[_]): Schema[_] = {
      val honors    = p.honorsSchemaAnnotations
      val annotated = if (honors) f.discriminator else None
      val style     = annotated.fold(p.sumStyle)(d => SumStyle.Discriminator(d.field)) match {
        case SumStyle.PerFamily(choose) =>
          choose(f) match {
            case SumStyle.PerFamily(_) => throw new IllegalArgumentException(s"${p.name}: SumStyle.PerFamily chose another PerFamily for ${f.members.map(_.fullName).mkString("family(", ", ", ")")}")
            case chosen                => chosen
          }
        case other => other
      }
      val tagField = style match {
        case SumStyle.Discriminator(x) => Some(x)
        case _                         => None
      }

      def label(m: `family`.Member): String =
        if (annotated.isDefined) m.discriminatorKey getOrElse m.sig
        else if (honors) m.discriminatorKey getOrElse p.memberName(m)
        else p.memberName(m)

      // the member's texts (and, honoring annotations, its `@discriminatorKey`) where its schema has none of its own
      def withTexts(m: `family`.Member, s: Schema[_]): Schema[_] =
        s.duplicate(
          title = s.title orElse m.title,
          description = s.description orElse m.description,
          discriminationKey = if (honors) s.discriminationKey orElse m.discriminatorKey else s.discriminationKey
        )

      def withDefinition(m: `family`.Member, s: Schema[_]): Schema[_] = m.definition match {
        case Some(`family`.Definition.Inferred) =>
          s match {
            case d: `def`[_] => d
            case other       => `def`[Any](m.sig, other)
          }
        case Some(`family`.Definition.Named(n)) =>
          s match {
            case d: `def`[_] => `def`[Any](n, d.tpe).withExtraFrom(d)
            case other       => `def`[Any](n, other)
          }
        case None => s
      }

      // the renderer puts the tag into an object among a discriminated oneOf, also into a definition's body
      def tagged(s: Schema[_], tag: String): Schema[_] = s match {
        case d @ `def`(sig, body) => `def`[Any](sig, tagged(body, tag)).withExtraFrom(d)
        case other                => other.duplicate(discriminationKey = Some(tag))
      }

      // an implicit Schema's body as an object: tagged in a copy, inline, so its definition stays as it is for other uses. Anything else can't carry the tag (R6A).
      def taggedProvided(m: `family`.Member, s: Schema[_], field: String): Schema[_] = s match {
        case o: `object`[_]   => o.duplicate(discriminationKey = Some(label(m)))
        case `def`(_, body)   => taggedProvided(m, body, field)
        case `value-class`(t) => taggedProvided(m, t, field)
        case other            =>
          throw new IllegalArgumentException(
            s"${p.name}: the member ${m.fullName} of a family tagged by '$field' has an implicit schema that isn't an object ($other), so the tag can't be added to it. " +
              s"Give ${m.name} an implicit Schema of an object, or the whole family a hand-written Schema."
          )
      }

      // a member that is written as an object of its own: a case class, a value class, an implicit-provided one, or (AsTagged) a singleton
      def shaped(m: `family`.Member, own: Schema[_]): Schema[_] = {
        val provided = m.kind == `family`.Kind.Provided
        val typed    = if (provided) own else withTexts(m, own)
        style match {
          case SumStyle.Untagged         => withDefinition(m, typed)
          case SumStyle.WrapperObject    => `object`.fromList[Any](List(`object`.Field(label(m), withDefinition(m, typed), required = true)))
          case SumStyle.Discriminator(x) =>
            // 0.7 keys the member node itself (honored annotations); a library profile makes sure the tag reaches the object
            if (honors) withDefinition(m, typed.duplicate(discriminationKey = Some(label(m))))
            else if (provided) taggedProvided(m, typed, x)
            else withDefinition(m, tagged(typed, label(m)))
          case SumStyle.PerFamily(_) => throw new IllegalStateException("resolved above") // `style` is never a PerFamily here
        }
      }

      // a value-class member: 0.7 and the honoring profiles inline its value; a library's derived sum codec writes the case class, `{"<field>": value}`
      def own(m: `family`.Member): Schema[_] = (m.kind, m.valueField, apply(m.schema)) match {
        case (`family`.Kind.ValueClass, Some(f), `value-class`(inner)) if !honors =>
          `object`.fromList[Any](List(`object`.Field(f, inner, required = true)))
        case (`family`.Kind.ValueClass, None, _) if !honors =>
          throw new IllegalArgumentException(s"${p.name}: the value-class member ${m.fullName} has no valueField, so it can't be written as the case class its library writes")
        case (_, _, s) => s
      }

      def singletonValue(m: `family`.Member): Value      = if (honors) m.value getOrElse Value.str(m.name) else Value.str(label(m))
      def singletonSchema(m: `family`.Member): Schema[_] = if (honors) apply(m.schema) else `string`[String](None)
      def constOf(m: `family`.Member): Schema[_]         = {
        val c = `const`[Any](singletonValue(m))
        c.duplicate(title = m.title, description = m.description, discriminationKey = if (honors) m.discriminatorKey else None)
      }

      sealed trait Entry
      final case class Single(schema: Schema[_])                           extends Entry
      final case class Merged(valueSchema: Schema[_], values: List[Value]) extends Entry

      val asString = p.singletonStyle == SingletonStyle.AsString
      val entries  = f.members.foldLeft(Vector.empty[Entry]) { (acc, m) =>
        if (m.kind == `family`.Kind.Singleton && asString) {
          if (f.enumsAsOneOf) acc :+ Single(constOf(m))
          // a ToValue-driven value without a @typeHint that isn't a string (`family.valueSchema`): that value, as a const
          else if (honors && m.schema.isInstanceOf[`const`[_]]) acc :+ Single(constOf(m))
          else {
            val vs = singletonSchema(m)
            acc.collectFirst { case e @ Merged(`vs`, _) => e } match {
              case Some(e) => acc.filterNot(_ == e) :+ e.copy(values = e.values :+ singletonValue(m))
              case None    => acc :+ Merged(vs, List(singletonValue(m)))
            }
          }
        } else if (m.kind == `family`.Kind.Singleton) acc :+ Single(shaped(m, `object`.fromList[Any](Nil)))
        else acc :+ Single(shaped(m, own(m)))
      }
      val schemas = entries.toList map {
        case Single(s)          => s
        case Merged(vs, values) => `enum`[Any](vs, values.toSet)
      }

      val singletonsOnly = asString && f.members.nonEmpty && f.members.forall(_.kind == `family`.Kind.Singleton)
      schemas match {
        case List(e: `enum`[_]) if singletonsOnly => e
        case _ if singletonsOnly                  => `oneof`[Any](schemas.toSet, None)
        case _                                    => `oneof`[Any](schemas.toSet, tagField)
      }
    }
  }
}

/** Holds the fallback profile. A parent of [[JsonSchemaProfile]] so that the fallback sits in the implicit scope of `JsonSchemaProfile` but not in `object JsonSchemaProfile`: an
  * `import JsonSchemaProfile._` next to a user's implicit profile is then not ambiguous. Not meant to be used directly.
  */
trait JsonSchemaProfileDefault
object JsonSchemaProfileDefault {

  /** No profile in scope: [[JsonSchemaProfile.Neutral]]. */
  implicit def neutral: JsonSchemaProfile = JsonSchemaProfile.Neutral
}
