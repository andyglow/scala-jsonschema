package com.github.andyglow.jsonschema

case class TypeSignature[+T](signature: String)

object TypeSignature {

  def unsafe(name: String): TypeSignature[_] = TypeSignature[Any](name)
}
