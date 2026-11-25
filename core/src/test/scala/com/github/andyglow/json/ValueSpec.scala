package com.github.andyglow.json

import Value._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class ValueSpec extends AnyWordSpec {

  "Value" when {

    "it comes to arrays" should {

      "handle empty" in {
        arr.empty shouldEqual arr()
      }

      "handle ++" in {
        arr() ++ arr("z") shouldEqual arr("z")
        arr(1) ++ arr("z", 15) shouldEqual arr(1, "z", 15)
      }

      "handle :+" in {
        arr() :+ "z" shouldEqual arr("z")
        arr(1) :+ "z" shouldEqual arr(1, "z")
      }

      "handle append" in {
        arr() append "z" shouldEqual arr("z")
        arr(1) append "z" shouldEqual arr(1, "z")
      }

      "handle +:" in {
        "z" +: arr() shouldEqual arr("z")
        "z" +: arr(1) shouldEqual arr("z", 1)
      }

      "handle prepend" in {
        arr() prepend "z" shouldEqual arr("z")
        arr(1) prepend "z" shouldEqual arr("z", 1)
      }
    }

    "it comes to objects" should {

      "handle empty" in {
        obj.empty shouldEqual obj()
      }

      "handle keys" in {
        obj("foo" -> "foo", "bar" -> "bar").keys should contain.only("foo", "bar")
      }

      "handle values" in {
        obj("foo" -> "foo", "bar" -> "bar").values should contain.only(str("foo"), str("bar"))
      }

      "handle value" in {
        obj("foo" -> "foo", "bar" -> "bar").value shouldEqual Map(
          "foo" -> str("foo"),
          "bar" -> str("bar")
        )
      }

      "handle fieldSet" in {
        obj("foo" -> "foo", "bar" -> "bar").fieldSet.toMap shouldEqual Map(
          "foo" -> str("foo"),
          "bar" -> str("bar")
        )
      }

      "handle fields" in {
        obj("foo" -> "foo", "bar" -> "bar").fields.toMap shouldEqual Map(
          "foo" -> str("foo"),
          "bar" -> str("bar")
        )
      }

      "handle -" in {
        obj("foo" -> "foo", "bar" -> "bar") - "bar" shouldEqual obj("foo" -> "foo")
      }

      "handle +" in {
        obj("foo" -> "foo") + ("bar" -> "bar") shouldEqual obj("foo" -> "foo", "bar" -> "bar")
      }

      "handle ++" in {
        obj("foo" -> "foo") ++ obj("bar" -> "bar") shouldEqual obj("foo" -> "foo", "bar" -> "bar")
      }

      "handle ++option" in {
        obj("foo" -> "foo") ++ Some(obj("bar" -> "bar")) shouldEqual obj(
          "foo" -> "foo",
          "bar" -> "bar"
        )

        obj("foo" -> "foo") ++ None shouldEqual obj("foo" -> "foo")
      }

      "handle deepMerge" in {
        val o1 = obj("field" -> obj("foo" -> "foo1", "bar" -> "bar"))

        val o2 = obj("field" -> obj("foo" -> "foo2", "baz" -> "baz"))

        o1 deepMerge o2 shouldEqual obj(
          "field" -> obj("foo" -> "foo2", "baz" -> "baz", "bar" -> "bar")
        )
      }
    }

    // Scala's immutable Map keeps insertion order up to 4 entries only, so these use 6
    "it comes to the order of object keys" should {
      def keys(o: obj): List[String] = o.fields.map(_._1).toList
      val six                        = obj("k1" -> 1, "k2" -> 2, "k3" -> 3, "k4" -> 4, "k5" -> 5, "k6" -> 6)
      val k16                        = List("k1", "k2", "k3", "k4", "k5", "k6")
      // built from an immutable (hash-ordered) Map: its own order is whatever the Map iterates
      val hashed  = obj(Map("h1" -> num(1), "h2" -> num(2), "h3" -> num(3), "h4" -> num(4), "h5" -> num(5)))
      val hashKey = keys(hashed)

      "keep it in +" in {
        keys(six + ("k7" -> num(7))) shouldBe k16 :+ "k7"
        keys(hashed + ("k7" -> num(7))) shouldBe hashKey :+ "k7"
      }

      "keep it in -" in {
        keys(six - "k3") shouldBe k16.filterNot(_ == "k3")
      }

      "keep it in ++" in {
        keys(six ++ obj("k7" -> 7, "k8" -> 8)) shouldBe k16 ++ List("k7", "k8")
        keys(hashed ++ six) shouldBe hashKey ++ k16
        keys(six ++ Some(obj("k7" -> 7))) shouldBe k16 :+ "k7"
      }

      "keep the left position and take the right value of a key on both sides of ++" in {
        val o = six ++ obj("k7" -> 7, "k2" -> 22)
        keys(o) shouldBe k16 :+ "k7"
        o.value("k2") shouldBe num(22)
      }

      "keep it in deepMerge" in {
        val o = obj("f" -> six, "g" -> 1) deepMerge obj("h" -> 2, "f" -> obj("k7" -> 7, "k2" -> 22))
        keys(o) shouldBe List("f", "g", "h")
        keys(o.value("f").asInstanceOf[obj]) shouldBe k16 :+ "k7"
        o.value("f").asInstanceOf[obj].value("k2") shouldBe num(22)
      }

      "keep it in toString" in {
        six.toString shouldBe """{"k1": 1, "k2": 2, "k3": 3, "k4": 4, "k5": 5, "k6": 6}"""
      }

      "keep it in JsonFormatter" in {
        JsonFormatter.format(six).replaceAll("\\s", "") shouldBe """{"k1":1,"k2":2,"k3":3,"k4":4,"k5":5,"k6":6}"""
      }
    }
  }
}
