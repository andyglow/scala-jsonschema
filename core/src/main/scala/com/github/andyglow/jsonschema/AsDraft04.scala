package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.schema.Version.Draft04

class AsDraft04(val v: Draft04) extends AsValue with AsDraftSupport {

  def schema(x: json.Schema[_]): obj = {
    val base = obj(
      f"$$schema"   -> v.uri,
      "description" -> x.description,
      "title"       -> x.title)

    val definitions = inferDefinitions(x)

    base ++ apply(x) ++ {
      if (definitions.fields.nonEmpty) obj("definitions" -> definitions) else obj()
    }
  }
}