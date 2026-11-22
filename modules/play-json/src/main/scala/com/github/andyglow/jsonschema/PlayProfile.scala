package com.github.andyglow.jsonschema

import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile._
import play.api.libs.json.{JsonConfiguration, JsonNaming}

/** The shapes play-json's macros (`Json.format`, `Json.writes`) write, read from the `JsonConfiguration` they were derived with.
  *
  *   - a sealed family: the member's own fields plus `discriminator` (default `_type`) set to `typeNaming(<the member's full class name>)`; a case object is `{"_type": "..."}`;
  *   - an `Option` field: `X` or `null`, not required. Both `OptionHandlers` (`Default` omits `None`, `WritesNull` writes `null`) read either form back, so one shape covers both;
  *   - `@discriminator` / `@discriminatorKey` don't apply (play's derivation never sees them); `@title`, `@description`, `@definition` do.
  *
  * `import com.github.andyglow.jsonschema.profiles.play._` gives this profile for the implicit `JsonConfiguration` in scope (play's default otherwise).
  */
object PlayProfile {

  /** The profile for `cfg`. Throws `IllegalArgumentException` if `cfg.naming` renames fields: the schema can't follow a field renaming. */
  def fromConfig(cfg: JsonConfiguration): JsonSchemaProfile = {
    renamed(cfg.naming) foreach { case (in, out) =>
      throw new IllegalArgumentException(
        s"PlayProfile: JsonConfiguration.naming renames fields ($in -> $out), which a JsonSchemaProfile can't describe. Use JsonNaming.Identity, or give the types an implicit json.Schema of your own"
      )
    }
    val memberName =
      if (cfg.typeNaming == JsonNaming.Identity) MemberName.FullName
      else MemberName.Custom(m => cfg.typeNaming(m.fullName))
    JsonSchemaProfile.Neutral
      .withName("play")
      .withSum(SumStyle.Discriminator(cfg.discriminator))
      .withSingleton(SingletonStyle.AsTagged)
      .withMemberName(memberName)
      .withHonorsSchemaAnnotations(false)
  }

  /** `fromConfig(JsonConfiguration.default)`: `_type`, full class names. */
  val default: JsonSchemaProfile = fromConfig(JsonConfiguration.default)

  // the first of some typical field names that `f` changes, and what it changes it to (a function can't be compared with `identity`)
  private def renamed(f: String => String): Option[(String, String)] =
    List("name", "fooBar", "FooBar", "foo_bar", "foo-bar", "x1", "URL", "a").iterator.map(n => n -> f(n)).find { case (in, out) => in != out }
}
