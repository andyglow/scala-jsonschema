package com.github.andyglow.jsonschema

import com.github.andyglow.json.ToValue
import json._
import json.Schema._
import json.schema._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

// need to come first as otherwise scala 2.11 fail with
// [error] knownDirectSubclasses of case0 observed before subclass V1 registered
// [error] knownDirectSubclasses of case0 observed before subclass V2 registered
object EnumSpec {

  sealed trait case0
  object case0 {
    case object V1 extends case0
    case object V2 extends case0
  }

  @typeHint[String]
  sealed trait case1
  object case1 {
    case object V1 extends case1
    case object V2 extends case1
  }

  @typeHint[String]
  sealed trait case2
  object case2 {
    @typeHint[String]
    case object V1 extends case2
    @typeHint[String]
    case object V2 extends case2
  }

  @typeHint[Int]
  sealed trait case3
  object case3 {
    case object V1 extends case3
    case object V2 extends case3
  }

  @typeHint[Int]
  sealed trait case4
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
    @typeHint[Int]
    case object V1 extends case6
    @typeHint[Int]
    case object V2 extends case6

    implicit val asValue: ToValue[case6] = ToValue mk {
      case V1 => 1
      case V2 => 2
    }
  }

  sealed trait case7
  object case7 {
    @typeHint[Int]
    case object V1 extends case7
    @typeHint[Double]
    case object V2 extends case7

    implicit val asValue: ToValue[case7] = ToValue mk {
      case V1 => 1
      case V2 => 2
    }
  }

  sealed trait case8
  object case8 {
    @title("v1")
    @description("Version 1")
    case object V1 extends case8

    @title("v2")
    @description("Version 2")
    case object V2 extends case8
  }
}

class EnumSpec extends AnyWordSpec {
  import EnumSpec._

  "`enum`" should {

    "be derived" when {

      "all case objects, no type hints" in {
        Json.schema[case0] shouldBe `enum`.of("V1", "V2")
      }

      "all case objects, no type hints, enum-as-oneof" in {
        implicit val jsonSchemaFlags: Flag with Flag.EnumsAsOneOf = null
        Json.schema[case0] shouldBe `oneof`.of[case0](
          const("V1"),
          const("V2")
        )
      }

      "all case objects, no type hints, enum-as-oneof decorated" in {
        implicit val jsonSchemaFlags: Flag with Flag.EnumsAsOneOf = null
        Json.schema[case8] shouldBe `oneof`.of[case8](
          const("V1").withTitle("v1").withDescription("Version 1"),
          const("V2").withTitle("v2").withDescription("Version 2")
        )
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
        val err = intercept[TestFailedException] { "Json.schema[case5]" should compile }
        err.getMessage shouldBe
          """Expected no compiler error, but got the following type error: "com.github.andyglow.jsonschema.EnumSpec.case5.V1.type: In order to convert this into enum item
            |the combination of ToValue and Writer/Encoder (from your json-library) is going to be used
            |```
            |EnumSpec.this.case5.asValue(V1)
            |```
            |This is the case when resulting json value is hard to reason about in compile time.
            |Use @typeHint annotation to specify correct type.
            |", for code: Json.schema[case5]""".stripMargin
      }

      "all case objects, ToValue is in implicit scope, Int for members type hint" in {
        Json.schema[case6] shouldBe `enum`.of(1, 2)
      }

      "all case objects, ToValue is in implicit scope, difference in type hints" in {
        val err = intercept[TestFailedException] { "Json.schema[case7]" should compile }
        err.getMessage shouldBe
          """Expected no compiler error, but got the following type error: "Error inferring schema for enum: com.github.andyglow.jsonschema.EnumSpec.case7. Some family members come with different schemas:
            |- json.schema.Predef.doubleS.schema:
            |  - com.github.andyglow.jsonschema.EnumSpec.case7.V2.type. Type Hint: Double
            |- json.schema.Predef.intS.schema:
            |  - com.github.andyglow.jsonschema.EnumSpec.case7.V1.type. Type Hint: Int", for code: Json.schema[case7]""".stripMargin
      }
    }
  }
}
