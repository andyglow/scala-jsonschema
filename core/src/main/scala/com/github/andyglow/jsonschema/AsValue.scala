package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.schema.Version

trait AsValue {

  def schema(tpe: json.Schema[_]): obj

  def apply(schema: json.Schema[_]): obj
}

object AsValue {

  def schema[V <: Version](x: json.Schema[_], v: V)(implicit asValue: AsValueBuilder[V]): obj =
    asValue(v).schema(x)
}
