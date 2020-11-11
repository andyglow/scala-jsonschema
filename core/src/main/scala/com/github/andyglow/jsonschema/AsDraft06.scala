package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.Schema._
import json.ValidationDef
import json.schema.Version._


class AsDraft06(val v: Draft06) extends AsValue with AsDraftSupport {

  def schema(x: json.Schema[_]): obj = {
    val base = obj(
      f"$$schema"   -> v.uri,
      f"$$id"       -> v.id,
      "description" -> x.description,
      "title"       -> x.title)

    val definitions = inferDefinitions(x)

    base ++ apply(x) ++ {
      if (definitions.fields.nonEmpty) obj("definitions" -> definitions) else obj()
    }
  }

  override def buildRef(ref: String): String = s"#$ref"

  override def inferDefinition(x: `ref`[_]): (String, obj) = {
    val ref = x.tpe.refName getOrElse x.sig
    ref -> (obj(f"$$id" -> s"#$ref") ++ apply(x.tpe))
  }
}