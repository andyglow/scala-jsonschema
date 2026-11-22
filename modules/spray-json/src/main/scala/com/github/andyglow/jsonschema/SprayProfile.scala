package com.github.andyglow.jsonschema

import json.schema.JsonSchemaProfile

/** spray-json derives products only (`jsonFormatN`); the format of a sealed family is written by hand, so only you know its shape.
  *
  * [[default]] has [[JsonSchemaProfile.Neutral]]'s knobs: an `Option` field is `X` or `null`, not required (`DefaultJsonProtocol` omits `None`, `NullOptions` writes `null`, and both read either
  * back); a family is an untagged `oneOf`. It honors `@discriminator` / `@discriminatorKey`, which describe what your format writes. State the rest with the builders:
  *
  * {{{
  * SprayProfile.default.withSum(SumStyle.Discriminator("type")).withMemberName(MemberName.SimpleName).withSingleton(SingletonStyle.AsTagged)
  * }}}
  */
object SprayProfile {

  val default: JsonSchemaProfile = JsonSchemaProfile.Neutral.withName("spray")
}
