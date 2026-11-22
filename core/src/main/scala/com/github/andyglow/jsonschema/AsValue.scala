package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.schema.{JsonSchemaProfile, Version}

trait AsValue {

  def schema(tpe: json.Schema[_]): obj

  def apply(schema: json.Schema[_]): obj
}

object AsValue {

  /** `x` rendered for version `v`, its `option` nodes shaped by the implicit profile in scope, or by [[JsonSchemaProfile.Neutral]] when there is none. */
  def schema[V <: Version](x: json.Schema[_], v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): obj =
    asValue(v).schema(profile.lower(x))
}
