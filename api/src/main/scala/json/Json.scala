package json

import com.github.andyglow.jsonschema.{SchemaMacro, TypeSignature, TypeSignatureMacro}

import scala.language.experimental.macros

object Json {

  def schema[T]: Schema[T] = macro SchemaMacro.impl[T]

  def sig[T]: TypeSignature[T] = macro TypeSignatureMacro.impl[T]
}
