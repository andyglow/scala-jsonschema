package com.github.andyglow.jsonschema.profiles

import com.github.andyglow.jsonschema.CirceProfile
import json.schema.JsonSchemaProfile

/** Scala 3: a `given io.circe.derivation.Configuration` in scope (what `ConfiguredCodec` uses) wins over the semiauto profile. */
trait CirceProfiles extends CirceLowPriorityProfiles {

  @scala.annotation.implicitAmbiguous(
    "profiles.circe.* already provides a JsonSchemaProfile (from the given io.circe.derivation.Configuration in scope). To use your own profile, import com.github.andyglow.jsonschema.AsCirce.* instead, or use schema.withProfile(p)."
  )
  implicit def circeConfiguredJsonSchemaProfile(implicit c: io.circe.derivation.Configuration): JsonSchemaProfile = CirceProfile.fromConfig(c)
}
