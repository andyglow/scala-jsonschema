package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.schema.Version.Draft04

class AsDraft04(val v: Draft04) extends AsValue with AsDraftSupport with Pre09 {

  def schema(x: json.Schema[_]): obj = render(lowered(x))

  private def render(x: json.Schema[_]): obj = withDefinitionNames(x) {
    val base = obj(f"$$schema" -> v.uri)

    val definitions = inferDefinitions(x)

    base ++ apply(x) ++ {
      if (definitions.fields.nonEmpty) obj("definitions" -> definitions) else obj()
    }
  }

  protected def sanitizeDefinitionName(sig: String): String = DefinitionNames.uriFragment(sig)

  /** Draft 04 has no `propertyNames`: a key that matches no `patternProperties` pattern is rejected as an additional property (#466). */
  override protected def keyRestriction(pattern: String): obj = obj("additionalProperties" -> false)
}
