package json

import com.github.andyglow.jsonschema._
import json.Schema.`object`
import json.schema.{JsonSchemaProfile, Predef, Version}

import scala.language.experimental.macros

object Json {

  def schema[T]: Schema[T] = macro Macroses.deriveSchema[T]

  def objectSchema[T](decorations: (String, String)*): `object`[T] =
    macro Macroses.deriveObjectSchema[T]

  def sig[T]: TypeSignature[T] = macro Macroses.deriveSignature[T]

  object auto {

    implicit def derived[T]: Predef[T] = macro Macroses.derivePredef[T]
  }

  /** `schema` rendered for `version`, as a string. Its `Option`s are shaped by the implicit [[json.schema.JsonSchemaProfile]] in scope, by default `JsonSchemaProfile.Neutral`. */
  def stringify[T, V <: Version](schema: Schema[T], version: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): String =
    schema.stringify(version)
}
