package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._

object AsRaw extends AsValue with AsDraftSupport with Post09 {

  def schema(x: json.Schema[_]): obj = {
    val base = obj(
      "description" -> x.description,
      "title"       -> x.title)

    val definitions = inferDefinitions(x)

    base ++ apply(x) ++ {
      if (definitions.fields.nonEmpty) obj("definitions" -> definitions) else obj()
    }
  }
}