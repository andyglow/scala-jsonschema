package com.github.andyglow.jsonschema

import org.scalatest.funsuite.AnyFunSuite

class NonEmptySeqReproSpec extends AnyFunSuite {
  test("repro: NonEmptySeq") {
    import cats.data.NonEmptySeq
    import CatsSupport._
    assertCompiles("json.Json.schema[NonEmptySeq[Int]]")
  }
}
