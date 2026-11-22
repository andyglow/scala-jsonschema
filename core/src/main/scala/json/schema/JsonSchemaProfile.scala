package json.schema

import com.github.andyglow.json.Value
import json.Schema
import json.Schema._

/** How the JSON a schema describes is shaped, for the facts the derivation records but doesn't decide.
  *
  * `Json.schema[T]` (and `derives json.Schema`) records facts that a JSON library decides how to encode. In 0.8 that is one fact: "this is an `Option`", kept as the `option` node in every position
  * (root, collection element, map value, field). A profile lowers those nodes to plain JSON Schema when the schema is rendered, or earlier with `schema.withProfile(profile)`. Everything else
  * (Predefs, hand-written schemas, all other derived nodes) passes through unchanged.
  *
  * Which profile applies to a render:
  *   1. `schema.withProfile(p)`: shaped already, so the render's profile finds nothing left to lower;
  *   1. otherwise an implicit `JsonSchemaProfile` in scope where the schema is rendered (`AsValue.schema`, `asPlay`, `asCirce`, ..., `stringify`);
  *   1. otherwise [[JsonSchemaProfile.Neutral]] (found in the implicit scope, so any profile in lexical scope wins over it).
  *
  * {{{
  * implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy   // where you render: the 0.7 output (Scala 3: given JsonSchemaProfile = ...)
  * Json.schema[Foo].withProfile(JsonSchemaProfile.Legacy)                // or per schema
  * }}}
  */
trait JsonSchemaProfile extends JsonSchemaProfileDefault {

  /** A name for messages and `toString`. */
  def name: String

  /** How `Option` is shaped. */
  def optionStyle: JsonSchemaProfile.OptionStyle

  /** Whether the wire-shape annotations of sum types (`@discriminator`, `@discriminatorKey`) apply under this profile. True for [[JsonSchemaProfile.Neutral]] and [[JsonSchemaProfile.Legacy]]. It has
    * no effect yet: sum types are not shaped by profiles in 0.8.
    */
  def honorsSchemaAnnotations: Boolean = true

  /** `schema` with every `option` node lowered to plain JSON Schema nodes. Idempotent: a schema without `option` nodes comes back equal. */
  final def lower[T](schema: Schema[T]): Schema[T] = JsonSchemaProfile.lower(optionStyle, schema)

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

  /** The default: the 0.7 shapes, with every `Option[X]` as "X or null". */
  val Neutral: JsonSchemaProfile = new Impl("neutral", OptionStyle.Nullable)

  /** The 0.7 output, byte for byte. */
  val Legacy: JsonSchemaProfile = new Impl("legacy", OptionStyle.Legacy)

  private final class Impl(val name: String, val optionStyle: OptionStyle) extends JsonSchemaProfile

  private def lower[T](style: OptionStyle, schema: Schema[T]): Schema[T] = new Lowering(style)(schema).asInstanceOf[Schema[T]]

  private final class Lowering(style: OptionStyle) {

    def apply(s: Schema[_]): Schema[_] = s match {
      case `option`(inner)  => optionAt(inner).withExtraFrom(s)
      case o: `object`[_]   => o.withFieldsUpdated { case f => field(f) }
      case `array`(c, u)    => `array`[Any, List](apply(c).asInstanceOf[Schema[Any]], u).withExtraFrom(s)
      case `dictionary`(v)  => `dictionary`[Any, Any, Map](apply(v).asInstanceOf[Schema[Any]]).withExtraFrom(s)
      case `oneof`(xs, df)  => `oneof`[Any](xs map apply, df).withExtraFrom(s)
      case `allof`(xs)      => `allof`[Any](xs map apply).withExtraFrom(s)
      case `not`(t)         => `not`[Any](apply(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
      case `def`(sig, t)    => `def`[Any](sig, apply(t)).withExtraFrom(s)
      case `value-class`(t) => `value-class`[Any, Any](apply(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
      case other            => other // leaves, enum, const, ref: no nested schema that could hold an `option`
    }

    private def field(f: `object`.Field[_]): `object`.Field[_] = f.tpe match {
      // 0.7 dropped one layer for an `Option` field; the macros make every `Option` field not required
      case `option`(inner) if style == OptionStyle.Legacy && !f.required => f.copy(tpe = apply(inner))
      case t                                                             => f.copy(tpe = apply(t))
    }

    /** An `option` node anywhere but in a Legacy `Option` field. */
    private def optionAt(inner: Schema[_]): Schema[_] = style match {
      case OptionStyle.Nullable =>
        `oneof`[Any](Set(apply(flatten(inner)), Schema.`null`))
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
