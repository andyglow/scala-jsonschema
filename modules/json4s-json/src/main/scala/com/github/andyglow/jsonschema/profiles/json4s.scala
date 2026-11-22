package com.github.andyglow.jsonschema.profiles

import com.github.andyglow.json.ToValue
import com.github.andyglow.jsonschema.{AsJson4s, AsValue, AsValueBuilder, Json4sProfile}
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.json4s.{Formats, Writer}
import org.json4s.JsonAST.JObject

/** `import com.github.andyglow.jsonschema.profiles.json4s._` (Scala 3: `.*`): what `AsJson4s._` gives (`asJson4s`, `ToValue` from `Writer`), plus the [[Json4sProfile]] of the implicit `Formats` in
  * scope (the one `Serialization.write` uses), else of `DefaultFormats`.
  *
  * Import either this or `AsJson4s._`, not both. To render with a profile of your own, import `AsJson4s._` and declare it, or use `schema.withProfile(p)`.
  */
object json4s extends Json4sLowPriorityProfiles {

  implicit def json4sToValue[T](implicit w: Writer[T]): ToValue[T] = AsJson4s.toValue[T]

  implicit final class Json4sProfileSchemaOps[T](private val x: Schema[T]) extends AnyVal {
    def asJson4s[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): JObject = AsJson4s(AsValue.schema(x, v)(asValue, profile))
  }

  @scala.annotation.implicitAmbiguous(
    "profiles.json4s._ already provides a JsonSchemaProfile (from the implicit Formats in scope). To use your own profile, import com.github.andyglow.jsonschema.AsJson4s._ instead, or use schema.withProfile(p)."
  )
  implicit def json4sJsonSchemaProfile(implicit f: Formats): JsonSchemaProfile = Json4sProfile.fromFormats(f)
}

trait Json4sLowPriorityProfiles {

  @scala.annotation.implicitAmbiguous(
    "profiles.json4s._ already provides a JsonSchemaProfile (Json4sProfile.default, no implicit Formats in scope). To use your own profile, import com.github.andyglow.jsonschema.AsJson4s._ instead, or use schema.withProfile(p)."
  )
  // a def with an implicit parameter, like json4sJsonSchemaProfile: on Scala 3 that makes it less specific than a profile you declare next to the import, which then wins
  implicit def json4sDefaultJsonSchemaProfile(implicit d: DummyImplicit): JsonSchemaProfile = Json4sProfile.default
}
