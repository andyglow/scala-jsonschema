package json

import com.github.andyglow.jsonschema._
import json.Schema.`object`
import json.schema.{Predef, Version}

import scala.language.experimental.macros


object Json {

  def schema[T]: Schema[T] = macro Macroses.deriveSchema[T]

  def objectSchema[T](decorations: (String, String)*): `object`[T] = macro Macroses.deriveObjectSchema[T]

  def sig[T]: TypeSignature[T] = macro Macroses.deriveSignature[T]

  object auto {

    implicit def derived[T]: Predef[T] = macro Macroses.derivePredef[T]
  }

  def stringify[T, V <: Version : AsValueBuilder](schema: Schema[T], version: V): String = schema.stringify(version)
}
