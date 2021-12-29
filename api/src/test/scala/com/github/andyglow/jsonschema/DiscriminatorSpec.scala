package com.github.andyglow.jsonschema

import json.{Schema => S, schema => s, Json}, S._, S.`object`.{Field => F}, s._, s.{Version => v}
import com.github.andyglow.json.Value._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._
import JsonMatchers._

// moved on top because of
// - knownDirectSubclasses of Pet observed before subclass Cat registered
object DiscriminatorSpec {

  // phantom
  object case0 {
    @discriminator sealed trait Root
    object Root {
      final case class Member1(value: Int)    extends Root
      final case class Member2(value: String) extends Root
    }
  }

  // not a phantom
  object case1 {
    @discriminator(field = "type", phantom = false) sealed trait Root
    object Root {
      final case class Member1(`type`: String, value: Int)    extends Root
      final case class Member2(`type`: String, value: String) extends Root
    }
  }

  // not a phantom, should fail because discriminator field is both
  // - not a phantom
  // - and not present
  object case2 {
    @discriminator(field = "tpe", phantom = false) sealed trait Root
    object Root {
      final case class Member1(`type`: String, value: Int)    extends Root
      final case class Member2(`type`: String, value: String) extends Root
    }
  }

  // not a phantom, non-standard discrimination field
  object case3 {
    @discriminator(field = "tpe", phantom = false) sealed trait Root
    object Root {
      final case class Member1(tpe: String, value: Int)    extends Root
      final case class Member2(tpe: String, value: String) extends Root
    }
  }

  // specific discriminator keys
  object case4 {
    @discriminator sealed trait Root
    object Root {
      @discriminatorKey("m1") final case class Member1(value: Int)    extends Root
      @discriminatorKey("m2") final case class Member2(value: String) extends Root
    }
  }

  // definition + specific discriminator keys
  object case5 {
    @discriminator sealed trait Root
    object Root {
      @definition("m1") @discriminatorKey("m1") final case class Member1(value: Int)    extends Root
      @definition("m2") @discriminatorKey("m2") final case class Member2(value: String) extends Root
    }
  }
}

class DiscriminatorSpec extends AnyWordSpec {
  import DiscriminatorSpec._

  "discriminator" should {

    "be exposed" when {

      "phantom, no specific keys" in {
        val s = Json.schema[case0.Root]
        s shouldBe `oneof`
          .of(
            `object`(F("value", `integer`, required = true)).withDiscriminationKey(
              "com.github.andyglow.jsonschema.DiscriminatorSpec.case0.Root.Member1"
            ),
            `object`(F("value", `string`, required = true)).withDiscriminationKey(
              "com.github.andyglow.jsonschema.DiscriminatorSpec.case0.Root.Member2"
            )
          )
          .discriminatedBy("type")

        AsValue.schema(s, v.Raw) should containJson {
          obj(
            "oneOf" -> arr(
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type" -> obj(
                    "enum" -> arr(
                      "com.github.andyglow.jsonschema.DiscriminatorSpec.case0.Root.Member1"
                    )
                  ),
                  "value" -> obj("type" -> "integer")
                )
              ),
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type" -> obj(
                    "enum" -> arr(
                      "com.github.andyglow.jsonschema.DiscriminatorSpec.case0.Root.Member2"
                    )
                  ),
                  "value" -> obj("type" -> "string")
                )
              )
            )
          )
        }
      }

      "non-phantom, no specific keys" in {
        val s = Json.schema[case1.Root]
        s shouldBe `oneof`
          .of(
            `object`(F("type", `string`, required = true), F("value", `integer`, required = true))
              .withDiscriminationKey(
                "com.github.andyglow.jsonschema.DiscriminatorSpec.case1.Root.Member1"
              ),
            `object`(F("type", `string`, required = true), F("value", `string`, required = true))
              .withDiscriminationKey(
                "com.github.andyglow.jsonschema.DiscriminatorSpec.case1.Root.Member2"
              )
          )
          .discriminatedBy("type")

        AsValue.schema(s, v.Raw) should containJson {
          obj(
            "oneOf" -> arr(
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type" -> obj(
                    "enum" -> arr(
                      "com.github.andyglow.jsonschema.DiscriminatorSpec.case1.Root.Member1"
                    )
                  ),
                  "value" -> obj("type" -> "integer")
                )
              ),
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type" -> obj(
                    "enum" -> arr(
                      "com.github.andyglow.jsonschema.DiscriminatorSpec.case1.Root.Member2"
                    )
                  ),
                  "value" -> obj("type" -> "string")
                )
              )
            )
          )
        }
      }

      "phantom, specific field, no specific keys" in {
        val s = Json.schema[case3.Root]
        s shouldBe `oneof`
          .of(
            `object`(F("tpe", `string`, required = true), F("value", `integer`, required = true))
              .withDiscriminationKey(
                "com.github.andyglow.jsonschema.DiscriminatorSpec.case3.Root.Member1"
              ),
            `object`(F("tpe", `string`, required = true), F("value", `string`, required = true))
              .withDiscriminationKey(
                "com.github.andyglow.jsonschema.DiscriminatorSpec.case3.Root.Member2"
              )
          )
          .discriminatedBy("tpe")

        AsValue.schema(s, v.Raw) should containJson {
          obj(
            "oneOf" -> arr(
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("tpe", "value"),
                "properties" -> obj(
                  "tpe" -> obj(
                    "enum" -> arr(
                      "com.github.andyglow.jsonschema.DiscriminatorSpec.case3.Root.Member1"
                    )
                  ),
                  "value" -> obj("type" -> "integer")
                )
              ),
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("tpe", "value"),
                "properties" -> obj(
                  "tpe" -> obj(
                    "enum" -> arr(
                      "com.github.andyglow.jsonschema.DiscriminatorSpec.case3.Root.Member2"
                    )
                  ),
                  "value" -> obj("type" -> "string")
                )
              )
            )
          )
        }
      }

      "phantom, specific keys" in {
        val s = Json.schema[case4.Root]
        s shouldBe `oneof`
          .of(
            `object`(F("value", `integer`, required = true)).withDiscriminationKey("m1"),
            `object`(F("value", `string`, required = true)).withDiscriminationKey("m2")
          )
          .discriminatedBy("type")

        AsValue.schema(s, v.Raw) should containJson {
          obj(
            "oneOf" -> arr(
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type"  -> obj("enum" -> arr("m1")),
                  "value" -> obj("type" -> "integer")
                )
              ),
              obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type"  -> obj("enum" -> arr("m2")),
                  "value" -> obj("type" -> "string")
                )
              )
            )
          )
        }
      }

      "definition, phantom, specific keys" in {
        val s = Json.schema[case5.Root]
        s shouldBe `oneof`
          .of(
            `def`(
              "m1",
              `object`(F("value", `integer`, required = true)).withDiscriminationKey("m1")
            ),
            `def`("m2", `object`(F("value", `string`, required = true)).withDiscriminationKey("m2"))
          )
          .discriminatedBy("type")

        AsValue.schema(s, v.Raw) should containJson {
          obj(
            "oneOf" -> arr(
              obj(f"$$ref" -> "#/definitions/m1"),
              obj(f"$$ref" -> "#/definitions/m2")
            ),
            "definitions" -> obj(
              "m1" -> obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type"  -> obj("enum" -> arr("m1")),
                  "value" -> obj("type" -> "integer")
                )
              ),
              "m2" -> obj(
                "additionalProperties" -> false,
                "required"             -> arr("type", "value"),
                "properties" -> obj(
                  "type"  -> obj("enum" -> arr("m2")),
                  "value" -> obj("type" -> "string")
                )
              )
            )
          )
        }
      }
    }
  }

  "compiler" should {

    "fail" when {

      "non-phantom discriminator field is not present in some of members" in {
        val err = intercept[TestFailedException] {
          assertCompiles("Json.schema[case2.Root]")
        }
        err.getMessage shouldBe "Expected no compiler error, but got the following type error: \"Discriminator: Field 'tpe' is not found in com.github.andyglow.jsonschema.DiscriminatorSpec.case2.Root.Member1\", for code: Json.schema[case2.Root]"
      }
    }
  }
}
