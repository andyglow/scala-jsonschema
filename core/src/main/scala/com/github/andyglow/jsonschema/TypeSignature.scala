package com.github.andyglow.jsonschema

import scala.annotation.StaticAnnotation

case class TypeSignature[+T](signature: String) extends StaticAnnotation

object TypeSignature {

  def unsafe(name: String): TypeSignature[_] = TypeSignature[Any](name)
}
