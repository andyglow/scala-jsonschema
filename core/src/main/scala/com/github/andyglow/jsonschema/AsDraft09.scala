package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.Schema._
import json.schema.Version._


class AsDraft09(val v: Draft09) extends AsValue with AsDraftSupport with Post09 {

  def schema(x: json.Schema[_]): obj = {
    val base = obj(
      f"$$schema"   -> v.uri,
      f"$$id"       -> v.id)

    val definitions = inferDefinitions(x)

    base ++ {
      if (definitions.fields.nonEmpty) obj("$defs" -> definitions) else obj()
    } ++ apply(x)
  }

  override def buildRef(ref: String): String = s"#$ref"

  override def inferDefinition(x: `def`[_], par: ParentSchema): (String, obj) = {
    val ref = x.sig
    ref -> (obj(f"$$anchor" -> ref) ++ apply(x.tpe, par orElse Some(x), includeType = true, isRoot = false))
  }
}