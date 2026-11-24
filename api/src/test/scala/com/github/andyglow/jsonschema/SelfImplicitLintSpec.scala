package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// #320: `implicit val s: Schema[X] = Json.schema[X]` (the README's basic usage) made Scala 2.13's `-Xlint` (implicit-recursion) warn
// "Implicit resolves to enclosing value s": the macro's implicit search for Schema[X] found the val being defined, and then declined it.
// The 2.13 build reports that lint as an error (project/CompilerOptions.scala), so these models fail to compile if it comes back.
class SelfImplicitLintSpec extends AnyFunSuite {
  import SelfImplicitLintSpec._

  test("#320: the README pattern, with toDefinition") {
    Readme.userIdSchema shouldEqual `def`[UserId]("userId", `string`)
  }

  test("#320: a comment inside the type, the type on its own line") {
    Commented.userIdSchema shouldEqual `def`[UserId]("userId", `string`)
  }

  test("#320: an implicit in the companion (found through the implicit scope)") {
    Account.accountSchema shouldEqual `object`[Account](Field("id", `string`))
  }

  test("#320: an implicit def with a type parameter") {
    // (only the shape: how the definition of `T` is named is #297)
    Generic.boxSchema[Int](`integer`) match {
      case o: `object`[_] => o.fields.map(_.name) shouldEqual Seq("value")
      case other          => fail(s"not an object: $other")
    }
  }
}

object SelfImplicitLintSpec {
  case class UserId(value: String) extends AnyVal

  object Readme {
    implicit val userIdSchema: json.Schema[UserId] = Json.schema[UserId].toDefinition("userId")
  }

  object Commented {
    implicit val userIdSchema: json.Schema[ // a comment
      UserId
    ] = Json.schema[UserId].toDefinition("userId")
  }

  case class Account(id: UserId)
  object Account {
    implicit val accountSchema: Schema[Account] = Json.schema[Account]
  }

  case class Box[T](value: T)
  object Generic {
    implicit def boxSchema[T](implicit t: Schema[T]): Schema[Box[T]] = Json.schema[Box[T]]
  }
}
