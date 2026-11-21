package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.Golden
import json._
import org.scalatest.funsuite.AnyFunSuite

/** Characterization that only compiles on 2.13 (and is expected on Scala 3). */
class Scala213PlusCharSpec extends AnyFunSuite {

  test("A4: @discriminator with named args out of order") {
    Golden.check("char/fields/DiscOrder", Json.schema[DiscOrder.Disc])
  }
}
