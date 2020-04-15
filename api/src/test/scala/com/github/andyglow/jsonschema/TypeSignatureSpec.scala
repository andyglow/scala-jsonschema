package com.github.andyglow.jsonschema

import json.Json
import org.scalatest._
import org.scalatest.Matchers._

class TypeSignatureSpec extends WordSpec {
  import TypeSignatureSpec._

  "Json.sig" should {
    "return proper type signatures" in {

      Json.sig[String].signature shouldEqual "scala.Predef.String"

      Json.sig[Int].signature shouldEqual "scala.Int"

      Json.sig[Int].signature shouldEqual "scala.Int"

      Json.sig[Z].signature shouldEqual "com.github.andyglow.jsonschema.TypeSignatureSpec.Z"

      Json.sig[Y.type].signature shouldEqual "com.github.andyglow.jsonschema.TypeSignatureSpec.Y"

      Json.sig[Map[String, String]].signature shouldEqual "scala.Predef.Map[scala.Predef.String,scala.Predef.String]"

      Json.sig[X[BigDecimal]].signature shouldEqual "com.github.andyglow.jsonschema.TypeSignatureSpec.X[scala.BigDecimal]"

      Json.sig[Value].signature shouldEqual "com.github.andyglow.jsonschema.TypeSignatureSpec.Value"
    }
  }
}

object TypeSignatureSpec {

  case class Z()

  case object Y

  case class X[T](foo: T)

  case class Value(x: String) extends AnyVal
}