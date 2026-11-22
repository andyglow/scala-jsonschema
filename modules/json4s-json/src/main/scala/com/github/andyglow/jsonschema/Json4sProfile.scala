package com.github.andyglow.jsonschema

import json.Schema.`family`
import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile._
import org.json4s.{DefaultFormats, Formats, TypeHints}

/** The shapes json4s' `Extraction.decompose` / `Serialization.write` write under a `Formats`.
  *
  *   - a sealed family whose classes have no type hint: its members untagged, a case object as `{}`;
  *   - a family whose classes all have one (`ShortTypeHints`, `FullTypeHints`, ...): the member's own fields plus `"<hint field>": hintFor(<its class>)`, the field being the one the hints declare for
  *     that class (json4s 4 keeps it per class); a case object is `{"<hint field>": "..."}`. The macro records each member's runtime class for this;
  *   - an `Option` field: `X` or `null`, not required (`None` is omitted, `null` reads back as `None`);
  *   - `@discriminator` / `@discriminatorKey` don't apply (json4s never sees them); `@title`, `@description`, `@definition` do.
  *
  * A family json4s writes in a way no profile describes fails its render with an `IllegalArgumentException` naming the members: hints for only some of its classes, or hints with different fields. On
  * Scala 3, json4s 4 can't serialize case classes (no ScalaSig), and a Scala 3 `enum` case has no class of its own.
  *
  * `import com.github.andyglow.jsonschema.profiles.json4s._` gives the profile of the implicit `Formats` in scope (the one `Serialization.write` uses), else of `DefaultFormats`.
  */
object Json4sProfile {

  def fromFormats(f: Formats): JsonSchemaProfile = {
    val hints = f.typeHints
    val base  = JsonSchemaProfile.Neutral.withName("json4s").withSingleton(SingletonStyle.AsTagged).withHonorsSchemaAnnotations(false)
    if (hints.hints.isEmpty) base
    else
      base
        .withSum(SumStyle.PerFamily(styleOf(hints, _)))
        .withMemberName(MemberName.Custom(m => m.cls.flatMap(hints.hintFor) getOrElse m.sig))
  }

  /** `fromFormats(DefaultFormats)`: no type hints. */
  val default: JsonSchemaProfile = fromFormats(DefaultFormats)

  private def styleOf(hints: TypeHints, f: `family`[_]): SumStyle = {
    val (hinted, plain)                  = f.members.partition(_.cls exists hints.containsHint)
    def names(ms: List[`family`.Member]) = ms.map(_.fullName).mkString(", ")
    if (hinted.isEmpty) SumStyle.Untagged
    else if (plain.nonEmpty)
      throw new IllegalArgumentException(
        s"Json4sProfile: Formats.typeHints cover only some members of a sealed family (with a hint: ${names(hinted)}; without: ${names(plain)}). json4s writes the ones tagged and the others " +
          "untagged, which a JsonSchemaProfile can't describe. Give all of them a hint, or none"
      )
    else {
      val fields = hinted.map(m => m -> m.cls.flatMap(hints.typeHintFieldNameForClass).getOrElse(hints.typeHintFieldName))
      fields.map(_._2).distinct match {
        case List(field) => SumStyle.Discriminator(field)
        case _           =>
          throw new IllegalArgumentException(
            s"Json4sProfile: the type hints of a sealed family's members use different fields (${fields.map { case (m, x) => s"${m.fullName}: $x" }.mkString(", ")}), which a JsonSchemaProfile " +
              "can't describe. Use one typeHintFieldName for the family"
          )
      }
    }
  }
}
