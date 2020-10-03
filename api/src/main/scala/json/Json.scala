package json

import com.github.andyglow.jsonschema.{SchemaMacro, TypeSignature, TypeSignatureMacro}
import json.Schema.`object`
import json.schema.Predef

import scala.language.experimental.macros


object Json {

  def schema[T]: Schema[T] = macro SchemaMacro.implSchema[T]

  def objectSchema[T](descriptions: (String, String)*): `object`[T] = macro SchemaMacro.implObject[T]

  def sig[T]: TypeSignature[T] = macro TypeSignatureMacro.impl[T]

  object auto {

    implicit def derived[T]: Predef[T] = macro SchemaMacro.implPredef[T]
  }
}
