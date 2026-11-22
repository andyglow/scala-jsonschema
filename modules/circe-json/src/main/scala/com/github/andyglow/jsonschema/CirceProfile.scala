package com.github.andyglow.jsonschema

import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile._

/** The shapes circe's derived encoders write.
  *
  *   - [[default]], circe-generic (`deriveEncoder`, `deriveCodec`, `generic.auto`): a sealed family member is wrapped in an object named after it, `{"Dog": {...}}`; a case object is `{"Fish": {}}`;
  *   - [[configured]], a configured codec (circe-generic-extras on Scala 2, `ConfiguredCodec` with `io.circe.derivation.Configuration` on Scala 3): with a `discriminator`, the member's own fields
  *     plus `"<discriminator>": "<name>"`; without one, wrapped as above; names are `transformConstructorNames(<simple name>)`;
  *   - an `Option` field: `X` or `null`, not required (the encoders write `null`, the decoders accept a missing key too);
  *   - `@discriminator` / `@discriminatorKey` don't apply (circe's derivation never sees them); `@title`, `@description`, `@definition` do.
  *
  * `import com.github.andyglow.jsonschema.profiles.circe._` gives [[default]]; on Scala 3, a `given io.circe.derivation.Configuration` in scope wins (see `fromConfig`).
  */
object CirceProfile extends CirceProfilePlatform {

  /** The profile of a configured codec, from its configuration's fields (circe-generic-extras on Scala 2: `configured(cfg.discriminator, cfg.transformConstructorNames, cfg.transformMemberNames)`).
    * Throws `IllegalArgumentException` if `transformMemberNames` renames fields: the schema can't follow a field renaming. `useDefaults` needs nothing: a field with a default isn't required anyway.
    */
  def configured(discriminator: Option[String] = None, transformConstructorNames: String => String = identity, transformMemberNames: String => String = identity): JsonSchemaProfile = {
    renamed(transformMemberNames) foreach { case (in, out) =>
      throw new IllegalArgumentException(
        s"CirceProfile: transformMemberNames renames fields ($in -> $out), which a JsonSchemaProfile can't describe. Give the types an implicit json.Schema of your own"
      )
    }
    val memberName = renamed(transformConstructorNames).fold[MemberName](MemberName.SimpleName)(_ => MemberName.Custom(m => transformConstructorNames(m.name)))
    JsonSchemaProfile.Neutral
      .withName("circe")
      .withSum(discriminator.fold[SumStyle](SumStyle.WrapperObject)(SumStyle.Discriminator(_)))
      .withSingleton(SingletonStyle.AsTagged)
      .withMemberName(memberName)
      .withHonorsSchemaAnnotations(false)
  }

  /** circe-generic: wrapper objects, simple names. */
  val default: JsonSchemaProfile = configured()

  // the first of some typical names that `f` changes, and what it changes it to (a function can't be compared with `identity`)
  private def renamed(f: String => String): Option[(String, String)] =
    List("name", "fooBar", "FooBar", "foo_bar", "foo-bar", "x1", "URL", "a").iterator.map(n => n -> f(n)).find { case (in, out) => in != out }
}
