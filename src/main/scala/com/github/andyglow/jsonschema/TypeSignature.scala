package com.github.andyglow.jsonschema

import scala.language.experimental.macros

case class TypeSignature[T](signature: String)

object TypeSignature {

  def apply[T]: TypeSignature[T] = macro TypeSignatureMacro.impl[T]
}
