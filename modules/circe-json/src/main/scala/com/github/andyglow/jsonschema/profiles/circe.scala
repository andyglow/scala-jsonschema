package com.github.andyglow.jsonschema.profiles

import com.github.andyglow.json.ToValue
import com.github.andyglow.jsonschema.{AsCirce, AsValue, AsValueBuilder, CirceProfile}
import io.circe.{Encoder, Json}
import json.Schema
import json.schema.{JsonSchemaProfile, Version}

/** `import com.github.andyglow.jsonschema.profiles.circe._` (Scala 3: `.*`): what `AsCirce._` gives (`asCirce`, `ToValue` from `Encoder`), plus [[CirceProfile.default]] (circe-generic's wrapper
  * objects). On Scala 3, a `given io.circe.derivation.Configuration` in scope (what `ConfiguredCodec` derives with) wins: `CirceProfile.fromConfig(it)`.
  *
  * Import either this or `AsCirce._`, not both. For circe-generic-extras on Scala 2, import `AsCirce._` and declare `CirceProfile.configured(...)`, or use `schema.withProfile(p)`.
  */
object circe extends CirceProfiles {

  implicit def circeToValue[T](implicit w: Encoder[T]): ToValue[T] = AsCirce.toValue[T]

  implicit final class CirceProfileSchemaOps[T](private val x: Schema[T]) extends AnyVal {
    def asCirce[V <: Version](v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): Json = AsCirce(AsValue.schema(x, v)(asValue, profile))
  }
}

trait CirceLowPriorityProfiles {

  @scala.annotation.implicitAmbiguous(
    "profiles.circe._ already provides a JsonSchemaProfile (circe-generic's). To use your own profile, import com.github.andyglow.jsonschema.AsCirce._ instead, or use schema.withProfile(p)."
  )
  // a def with an implicit parameter, like the bridges that read a library config: on Scala 3 that makes it less specific than a profile you declare next to the import, which then wins
  implicit def circeJsonSchemaProfile(implicit d: DummyImplicit): JsonSchemaProfile = CirceProfile.default
}
