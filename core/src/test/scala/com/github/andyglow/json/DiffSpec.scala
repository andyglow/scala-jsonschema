package com.github.andyglow.json

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import comparison._
import comparison.Result._
import comparison.Diff._

class DiffSpec extends AnyWordSpec {
  import Value._

  "diff" should {

    "be computed" when {

      "primitives are equal" in {
        str("1").diff("1") shouldBe Equal
        `null`.diff(`null`) shouldBe Equal
        `true`.diff(`true`) shouldBe Equal
        `false`.diff(`false`) shouldBe Equal
        num(5).diff(5) shouldBe Equal
      }

      "primitives has same types but different values" in {
        str("1").diff("2") shouldBe Different(ValueMismatch(Path.Empty, "1", "2"))
        `true`.diff(`false`) shouldBe Different(ValueMismatch(Path.Empty, true, false))
        num(5).diff(15) shouldBe Different(ValueMismatch(Path.Empty, 5, 15))
        `null`.diff(15) shouldBe Different(ValueMismatch(Path.Empty, `null`, num(15)))
      }

      "primitives has different types" in {
        str("1").diff(`null`) shouldBe Different(TypeMismatch(Path.Empty, "string", "null"))
        str("1").diff(2) shouldBe Different(TypeMismatch(Path.Empty, "string", "number"))
        `true`.diff("abc") shouldBe Different(TypeMismatch(Path.Empty, "boolean", "string"))
        num(5).diff("abc") shouldBe Different(TypeMismatch(Path.Empty, "number", "string"))
      }

      "structural types" when {

        "objects are equal" in {
          obj("a" -> "b").diff(obj("a" -> "b")) shouldBe Equal
          obj("a" -> `true`).diff(obj("a" -> `true`)) shouldBe Equal
          obj("a" -> `false`).diff(obj("a" -> `false`)) shouldBe Equal
          obj("a" -> 15).diff(obj("a" -> 15)) shouldBe Equal
        }

        "objects are different" in {
          obj("a" -> "b", "c" -> "d").diff(
            obj(
              "b" -> "c"
            )
          ) shouldBe Different(
            MissingProperty("b", "c"),
            MissingProperty("a", "b"),
            MissingProperty("c", "d")
          )
        }

        "arrays are equal" in {
          arr(1, 2, 3).diff(arr(1, 2, 3)) shouldBe Equal
          arr("1", "2", "3").diff(arr("1", "2", "3")) shouldBe Equal
        }
      }
    }
  }
}
