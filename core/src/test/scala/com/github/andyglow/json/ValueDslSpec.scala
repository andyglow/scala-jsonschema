package com.github.andyglow.json

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.github.andyglow.json.Value._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.propspec.AnyPropSpec

class ValueDslSpec extends AnyPropSpec {

  val some: Option[String] = Some("15")

  val none: Option[String] = None

  val examples = Table(
    ("DSL", "Expected"),
    (obj("foo" -> true), obj("foo" -> `true`)),
    (obj("foo" -> false), obj("foo" -> `false`)),
    (obj("foo" -> "bar"), obj("foo" -> str("bar"))),
    (obj("foo" -> 15), obj("foo" -> num(15))),
    (obj("foo" -> some, "bar" -> none), obj("foo" -> str("15"))),
    (arr(1, 2), arr(num(1), num(2))),
    (arr("str", 2), arr(str("str"), num(2)))
  )

  property("check is DSL works well") {
    forAll(examples) { (js1, js2) => js1 shouldEqual js2 }
  }
}
