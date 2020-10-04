package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import json._
import json.schema.Version.Draft07
import json.schema._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

class DocumentationSpec extends AnyWordSpec {
  import DocumentationSpec._

  private val fromScaladoc    = Json.objectSchema[FromScaladoc]()
  private val fromAnnotations = Json.objectSchema[FromAnnotations]()
  private val fromConfig      = Json.objectSchema[FromConfig](
                                  "a" -> "A Param",
                                  "b" -> "B Param"
                                ) .withDescription("My perfect class")
                                  .withTitle("A Title")

  private def printSchema[T](s: Schema[T]): Unit = {
    val str = JsonFormatter.format(AsValue.schema(s, Draft07("foo-id")))
    println(str)
  }

  printSchema(fromAnnotations)

  "description" should {

    "be shown" when {

      "specified in scaladoc" in {
        fromScaladoc.description shouldBe Some("My perfect class")
        fromScaladoc.fields.flatMap(f => f.description map { d => f.name -> d }).toMap should contain only (
          "a" -> "A Param",
          "b" -> "B Param")
      }

      "specified in annotations" in {
        fromAnnotations.description shouldBe Some("My perfect class")
        fromAnnotations.fields.flatMap(f => f.description map { d => f.name -> d }).toMap should contain only (
          "a" -> "A Param",
          "b" -> "B Param")
      }

      "specified in config" in {
        fromConfig.description shouldBe Some("My perfect class")
        fromConfig.fields.flatMap(f => f.description map { d => f.name -> d }).toMap should contain only (
          "a" -> "A Param",
          "b" -> "B Param")
      }
    }
  }

  "title" should {

    "be shown" when {

      "specified in annotations" in {
        fromAnnotations.title shouldBe Some("A Title")
      }

      "specified in config" in {
        fromConfig.title shouldBe Some("A Title")
      }
    }
  }
}

object DocumentationSpec {

  /** My perfect class
    *
    * @param a A Param
    * @param b B Param
    */
  case class FromScaladoc(a: String, b: Int)

  @title("A Title")
  @description("My perfect class")
  case class FromAnnotations(
    @description("A Param") a: String,
    @description("B Param") b: Int)

  case class FromConfig(a: String, b: Int)
}
