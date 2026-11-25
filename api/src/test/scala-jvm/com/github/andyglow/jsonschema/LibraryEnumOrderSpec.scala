package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json._
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

// #458: the enum of a case-object family compiled in another module (`testmodels.LibraryColor`) keeps the derivation's
// order: by name on Scala 2 (no source positions in class files), declaration order on Scala 3 (EnumOrderExpected).
// JVM only: the derived code refers to the member objects, and on Scala.js the bridges' test runs link api's test
// classes without test-models on their classpath.
class LibraryEnumOrderSpec extends AnyWordSpec {

  "the enum of a family compiled in another module" should {
    "list its values in the derivation's member order" in {
      val rendered = AsValue.schema(Json.schema[testmodels.LibraryColor], Version.Draft07(id = "x"))
      rendered match {
        case o: obj => o.value("enum") shouldBe arr(EnumOrderExpected.libraryColor.map(str(_)))
        case other  => fail(other.toString)
      }
    }
  }
}
