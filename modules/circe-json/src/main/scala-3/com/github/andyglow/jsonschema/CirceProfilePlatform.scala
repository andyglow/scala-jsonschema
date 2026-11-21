package com.github.andyglow.jsonschema

import json.schema.JsonSchemaProfile

/** Scala 3: circe-core has the configuration `ConfiguredCodec` / `ConfiguredEncoder` derive with. */
trait CirceProfilePlatform {

  /** The profile of a codec derived with `c`. Throws `IllegalArgumentException` if `c.transformMemberNames` renames fields. */
  def fromConfig(c: io.circe.derivation.Configuration): JsonSchemaProfile =
    CirceProfile.configured(c.discriminator, c.transformConstructorNames, c.transformMemberNames)
}
