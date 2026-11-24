package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.harness.CompileErrors.outcome
import json._
import json.schema.{Version => v}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** E8 (misc types): java.util collections and Optional, Array[Byte], `@deprecated`. Repro: the target behavior. */
object MiscTypesSpec {
  case class User(name: String)

  case class JavaColls(
    list: java.util.List[Int],
    set: java.util.Set[String],
    users: java.util.List[User],
    map: java.util.Map[String, User],
    opt: java.util.Optional[User]
  )

  case class Bytes(data: Array[Byte])

  case class Old(@deprecated("use b", "1.0") a: Int, b: Int)
}

class MiscTypesSpec extends AnyFunSuite {
  import MiscTypesSpec._

  test("repro: java.util.List/Set/Map/Optional derive like their Scala counterparts") {
    outcome(assertCompiles("Json.schema[java.util.List[Int]]")) shouldBe None
    outcome(assertCompiles("Json.schema[java.util.Set[String]]")) shouldBe None
    outcome(assertCompiles("Json.schema[java.util.Map[String, Int]]")) shouldBe None
    outcome(assertCompiles("Json.schema[java.util.Optional[Int]]")) shouldBe None
    outcome(assertCompiles("Json.schema[JavaColls]")) shouldBe None
  }

  test("repro: Array[Byte] as today") {
    Json.schema[Bytes].stringify(v.Draft12("x")) shouldBe ""
  }

  test("repro: @deprecated field renders deprecated: true from 2019-09") {
    Json.schema[Old].stringify(v.Draft12("x")) should include(""""deprecated":true""")
  }
}
