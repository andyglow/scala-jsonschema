package com.github.andyglow.jsonschema

import json.Schema.`family`
import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile._

/** The shapes an upickle API object writes (`upickle.default`, or yours: `object MyPickler extends upickle.AttributeTagged { ... }`), read from its settings.
  *
  *   - a sealed family: the member's own fields plus `tagName` (default `$type`) set to the member's name (`objectTypeKeyWriteFullyQualified`: its full name), mapped by `objectTypeKeyWriteMap`; a
  *     case object is its name as a plain string (not mapped: upickle 4 maps only the tag);
  *   - an `Option` field: `X` or `null`. Its key is required unless the field has a default, because upickle's reader rejects a missing key (and its writer drops a field equal to its default);
  *   - `@discriminator` / `@discriminatorKey` don't apply (upickle's derivation never sees them), nor do upickle's own `@key` annotations (the schema derivation doesn't see those); `@title`,
  *     `@description`, `@definition` do.
  *
  * `import com.github.andyglow.jsonschema.profiles.upickle._` gives [[default]]. For your own API object, import `AsU._` and declare `implicit val profile = UPickleProfile.fromApi(MyPickler)`.
  */
object UPickleProfile {

  /** The profile for `api`. Throws `IllegalArgumentException` for settings a profile can't describe: `optionsAsNulls = false` (`Option`s as `[]` / `[x]`) and an `objectAttributeKeyWriteMap` that
    * renames fields.
    */
  def fromApi(api: _root_.upickle.AttributeTagged): JsonSchemaProfile = {
    if (!api.optionsAsNulls)
      throw new IllegalArgumentException("UPickleProfile: optionsAsNulls = false writes an Option as [] or [x], which a JsonSchemaProfile can't describe")
    renamed(n => api.objectAttributeKeyWriteMap(n).toString) foreach { case (in, out) =>
      throw new IllegalArgumentException(
        s"UPickleProfile: objectAttributeKeyWriteMap renames fields ($in -> $out), which a JsonSchemaProfile can't describe. Give the types an implicit json.Schema of your own"
      )
    }
    val name       = if (api.objectTypeKeyWriteFullyQualified) MemberName.FullName else MemberName.SimpleName
    val memberName = renamed(n => api.objectTypeKeyWriteMap(n).toString) match {
      case None => name
      // upickle 4 maps the tag of an object, not the string it writes for a case object
      case Some(_) => MemberName.Custom(m => if (m.kind == `family`.Kind.Singleton) name(m) else api.objectTypeKeyWriteMap(name(m)).toString)
    }
    JsonSchemaProfile.Neutral
      .withName("upickle")
      .withOptionKeyRequired(true)
      .withSum(SumStyle.Discriminator(api.tagName))
      .withSingleton(SingletonStyle.AsString)
      .withMemberName(memberName)
      .withHonorsSchemaAnnotations(false)
  }

  /** `fromApi(upickle.default)`: `$type`, simple names, case objects as strings. */
  val default: JsonSchemaProfile = fromApi(_root_.upickle.default)

  // the first of some typical names that `f` changes, and what it changes it to (a function can't be compared with `identity`)
  private def renamed(f: String => String): Option[(String, String)] =
    List("name", "fooBar", "FooBar", "foo_bar", "foo-bar", "x1", "URL", "a", "petshop.Dog").iterator.map(n => n -> f(n)).find { case (in, out) => in != out }
}
