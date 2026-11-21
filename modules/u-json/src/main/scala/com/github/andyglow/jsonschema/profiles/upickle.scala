package com.github.andyglow.jsonschema.profiles

import com.github.andyglow.json.ToValue
import com.github.andyglow.jsonschema.{AsU, AsValue, AsValueBuilder, UPickleProfile}
import json.Schema
import json.schema.{JsonSchemaProfile, Version}

/** `import com.github.andyglow.jsonschema.profiles.upickle._` (Scala 3: `.*`): what `AsU._` gives (`asU`, `ToValue` from `upickle.default.Writer`), plus [[UPickleProfile.default]], the profile of
  * `upickle.default`.
  *
  * Import either this or `AsU._`, not both. For an API object of your own, import `AsU._` and declare `UPickleProfile.fromApi(yourApi)`, or use `schema.withProfile(p)`.
  */
object upickle {

  // inside package `profiles`, `upickle` is this object: the library is `_root_.upickle`
  implicit def upickleToValue[T](implicit w: _root_.upickle.default.Writer[T]): ToValue[T] = AsU.toValue[T]

  implicit final class UPickleProfileSchemaOps[T](private val x: Schema[T]) extends AnyVal {
    def asU[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): ujson.Obj = AsU(AsValue.schema(x, v)(asValue, profile))
  }

  @scala.annotation.implicitAmbiguous(
    "profiles.upickle._ already provides a JsonSchemaProfile (the one of upickle.default). To use your own profile, import com.github.andyglow.jsonschema.AsU._ instead, or use schema.withProfile(p)."
  )
  // a def with an implicit parameter, like the bridges that read a library config: on Scala 3 that makes it less specific than a profile you declare next to the import, which then wins
  implicit def upickleJsonSchemaProfile(implicit d: DummyImplicit): JsonSchemaProfile = UPickleProfile.default
}
