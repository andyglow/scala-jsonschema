package com.github.andyglow.jsonschema.profiles

import com.github.andyglow.json.ToValue
import com.github.andyglow.jsonschema.{AsSpray, AsValue, AsValueBuilder, SprayProfile}
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import _root_.spray.json.{JsObject, JsonWriter}

/** `import com.github.andyglow.jsonschema.profiles.spray._` (Scala 3: `.*`): what `AsSpray._` gives (`asSpray`, `ToValue` from `JsonWriter`), plus [[SprayProfile.default]].
  *
  * Import either this or `AsSpray._`, not both. A family's format is yours: to describe it with a profile of your own, import `AsSpray._` and declare it, or use `schema.withProfile(p)`.
  */
object spray {

  implicit def sprayToValue[T](implicit w: JsonWriter[T]): ToValue[T] = AsSpray.toValue[T]

  implicit final class SprayProfileSchemaOps[T](private val x: Schema[T]) extends AnyVal {
    def asSpray[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): JsObject = AsSpray(AsValue.schema(x, v)(asValue, profile))
  }

  @scala.annotation.implicitAmbiguous(
    "profiles.spray._ already provides a JsonSchemaProfile (SprayProfile.default). To use your own profile, import com.github.andyglow.jsonschema.AsSpray._ instead, or use schema.withProfile(p)."
  )
  // a def with an implicit parameter, like the bridges that read a library config: on Scala 3 that makes it less specific than a profile you declare next to the import, which then wins
  implicit def sprayJsonSchemaProfile(implicit d: DummyImplicit): JsonSchemaProfile = SprayProfile.default
}
