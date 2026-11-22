package com.github.andyglow.jsonschema.profiles

import com.github.andyglow.json.ToValue
import com.github.andyglow.jsonschema.{AsPlay, AsValue, AsValueBuilder, PlayProfile}
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
// inside package `profiles`, `play` is this object: the library is `_root_.play`
import _root_.play.api.libs.json.{JsObject, JsonConfiguration, Writes}

/** `import com.github.andyglow.jsonschema.profiles.play._` (Scala 3: `.*`): what `AsPlay._` gives (`asPlay`, `ToValue` from `Writes`), plus the [[PlayProfile]] for the implicit `JsonConfiguration` in
  * scope, the one play's macros see there (play's own default if you declare none).
  *
  * Import either this or `AsPlay._`, not both. To render with a profile of your own, import `AsPlay._` and declare it, or use `schema.withProfile(p)`.
  */
object play {

  implicit def playToValue[T](implicit w: Writes[T]): ToValue[T] = AsPlay.toValue[T]

  implicit final class PlayProfileSchemaOps[T](private val x: Schema[T]) extends AnyVal {
    def asPlay[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): JsObject = AsPlay(AsValue.schema(x, v)(asValue, profile))
  }

  @scala.annotation.implicitAmbiguous(
    "profiles.play._ already provides a JsonSchemaProfile (from the implicit JsonConfiguration in scope). To use your own profile, import com.github.andyglow.jsonschema.AsPlay._ instead, or use schema.withProfile(p)."
  )
  implicit def playJsonSchemaProfile(implicit cfg: JsonConfiguration): JsonSchemaProfile = PlayProfile.fromConfig(cfg)
}
