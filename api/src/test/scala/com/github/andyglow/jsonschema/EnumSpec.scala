package com.github.andyglow.jsonschema

import com.github.andyglow.json.ToValue
import json._
import json.Schema._
import json.schema._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._


class EnumSpec extends AnyWordSpec {
  import EnumSpec._

  "`enum`" should {

    "be derived" when {

      "all case objects, no type hints" in {
        Json.schema[case0] shouldBe `enum`.of("V1", "V2")
      }

      "all case objects, string for root type hint" in {
        Json.schema[case1] shouldBe `enum`.of("V1", "V2")
      }

      "all case objects, string for root and all members type hint" in {
        Json.schema[case2] shouldBe `enum`.of("V1", "V2")
      }

      "all case objects, Int for root type hint" in {
        Json.schema[case3] shouldBe `enum`.of("V1", "V2")
      }

      "all case objects, ToValue is in implicit scope, Int for root type hint" in {
        Json.schema[case4] shouldBe `enum`.of(1, 2)
      }

      "all case objects, ToValue is in implicit scope, no type hints" in {
        "Json.schema[case5]" shouldNot compile
      }

      "all case objects, ToValue is in implicit scope, Int for members type hint" in {
        Json.schema[case6] shouldBe `enum`.of(1, 2)
      }

      "all case objects, ToValue is in implicit scope, Int for members type hint | xxx" in {
        "Json.schema[case7]" shouldNot compile
      }
    }
  }
}

object EnumSpec {

  sealed trait case0
  object case0 {
    case object V1 extends case0
    case object V2 extends case0
  }

  @typeHint[String] sealed trait case1
  object case1 {
    case object V1 extends case1
    case object V2 extends case1
  }

  @typeHint[String] sealed trait case2
  object case2 {
    @typeHint[String] case object V1 extends case2
    @typeHint[String] case object V2 extends case2
  }

  @typeHint[Int] sealed trait case3
  object case3 {
    case object V1 extends case3
    case object V2 extends case3
  }

  @typeHint[Int] sealed trait case4
  object case4 {
    case object V1 extends case4
    case object V2 extends case4

    implicit val asValue: ToValue[case4] = ToValue mk {
      case V1 => 1
      case V2 => 2
    }
  }

  sealed trait case5
  object case5 {
    case object V1 extends case5
    case object V2 extends case5

    implicit val asValue: ToValue[case5] = ToValue mk {
      case V1 => 1
      case V2 => 2
    }
  }

  sealed trait case6
  object case6 {
    @typeHint[Int] case object V1 extends case6
    @typeHint[Int] case object V2 extends case6

    implicit val asValue: ToValue[case6] = ToValue mk {
      case V1 => 1
      case V2 => 2
    }
  }

  sealed trait case7
  object case7 {
    @typeHint[Int] case object V1 extends case7
    @typeHint[Double] case object V2 extends case7

    implicit val asValue: ToValue[case7] = ToValue mk {
      case V1 => 1
      case V2 => 2
    }
  }
}
