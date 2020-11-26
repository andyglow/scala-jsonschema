package com.github.andyglow.json

import Value._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class ValueSpec extends AnyWordSpec {

  "Value" when {

    "it comes to arrays" should {

      "handle empty" in {
        arr.empty               shouldEqual arr()
      }

      "handle ++" in {
        arr() ++ arr("z")       shouldEqual arr("z")
        arr(1) ++ arr("z", 15)  shouldEqual arr(1, "z", 15)
      }

      "handle :+" in {
        arr() :+ "z"            shouldEqual arr("z")
        arr(1) :+ "z"           shouldEqual arr(1, "z")
      }

      "handle append" in {
        arr() append "z"        shouldEqual arr("z")
        arr(1) append "z"       shouldEqual arr(1, "z")
      }

      "handle +:" in {
        "z" +: arr()            shouldEqual arr("z")
        "z" +: arr(1)           shouldEqual arr("z", 1)
      }

      "handle prepend" in {
        arr() prepend "z"       shouldEqual arr("z")
        arr(1) prepend "z"      shouldEqual arr("z", 1)
      }
    }

    "it comes to objects" should {

      "handle empty" in {
        obj.empty shouldEqual obj()
      }

      "handle keys" in {
        obj(
          "foo" -> "foo",
          "bar" -> "bar").keys should contain.only("foo", "bar")
      }

      "handle values" in {
        obj(
          "foo" -> "foo",
          "bar" -> "bar").values should contain.only(str("foo"), str("bar"))
      }

      "handle value" in {
        obj(
          "foo" -> "foo",
          "bar" -> "bar").value shouldEqual Map(

          "foo" -> str("foo"),
          "bar" -> str("bar"))
      }

      "handle fieldSet" in {
        obj(
          "foo" -> "foo",
          "bar" -> "bar").fieldSet.toMap shouldEqual Map(

          "foo" -> str("foo"),
          "bar" -> str("bar"))
      }

      "handle fields" in {
        obj(
          "foo" -> "foo",
          "bar" -> "bar").fields.toMap shouldEqual Map(

          "foo" -> str("foo"),
          "bar" -> str("bar"))
      }

      "handle -" in {
        obj(
          "foo" -> "foo",
          "bar" -> "bar") - "bar"  shouldEqual obj("foo" -> "foo")
      }

      "handle +" in {
        obj("foo" -> "foo") + ("bar" -> "bar") shouldEqual obj(
          "foo" -> "foo",
          "bar" -> "bar")
      }

      "handle ++" in {
        obj("foo" -> "foo") ++ obj("bar" -> "bar") shouldEqual obj(
          "foo" -> "foo",
          "bar" -> "bar")
      }

      "handle ++option" in {
        obj("foo" -> "foo") ++ Some(obj("bar" -> "bar")) shouldEqual obj(
          "foo" -> "foo",
          "bar" -> "bar")

        obj("foo" -> "foo") ++ None shouldEqual obj(
          "foo" -> "foo")
      }

      "handle deepMerge" in {
        val o1 = obj("field" -> obj("foo" -> "foo1", "bar" -> "bar"))

        val o2 = obj("field" -> obj("foo" -> "foo2", "baz" -> "baz"))

        o1 deepMerge o2 shouldEqual obj(
          "field" -> obj(
            "foo" -> "foo2",
            "baz" -> "baz",
            "bar" -> "bar"))
      }
    }
  }
}
