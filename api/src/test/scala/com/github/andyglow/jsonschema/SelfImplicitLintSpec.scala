package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.annotation.nowarn

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
    Account.accountSchema shouldEqual `object`[Account](Field("id", Json.schema[UserId]))
  }

  test("#320: an implicit def with a type parameter") {
    // (only the shape: how the definition of `T` is named is #297)
    Generic.boxSchema[Int](`integer`) match {
      case o: `object`[_] => o.fields.map(_.name) shouldEqual Seq("value")
      case other          => fail(s"not an object: $other")
    }
  }

  // On 2.13.17+ the search leaves the definition being expanded out. What it finds instead is used only where it would have won against
  // that definition in the plain search (where the definition is found, declined, and the type derived). Same expectations on 2.12, 2.13, 3.
  private val derivedX = `object`.fromList[Any](List(Field("a", `integer`)))

  test("#320: a companion Schema loses to the definition being expanded: derived") {
    Mine1.mine shouldEqual derivedX
  }

  test("#320: a companion Predef loses to the Predef being expanded: derived") {
    Mine2.mine.schema shouldEqual derivedX
  }

  test("#320: a Schema of a base trait loses to the one defined in the object: derived") {
    Mine3.mine shouldEqual derivedX
  }

  test("#320: an imported, more specific Schema.object wins: a definition of it") {
    Mine5.mine shouldEqual `def`[X5](Json.sig[X5].signature, Other5.obj)
  }

  test("#320: a more specific Schema.object next to it wins: a definition of it") {
    Mine6.mine shouldEqual `def`[X6](Json.sig[X6].signature, Mine6.obj)
  }

  test("#320: a self Predef with Json.auto imported, in an object and in the companion: derived") {
    WithAuto.p.schema shouldEqual derivedX
    A2.p.schema shouldEqual derivedX
  }

  test("#320: a self Schema falls through to a Predef of the type, on every line layout") {
    TwoLineWithPredef.s shouldEqual `string`
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

  private val custom = Field("custom", `string`)

  case class X1(a: Int)
  object X1    { implicit val companion: Schema[X1] = `object`[X1](custom) }
  object Mine1 { implicit val mine: Schema[X1] = Json.schema[X1]           }

  case class X2(a: Int)
  object X2    { implicit val companion: Predef[X2] = Predef(`object`[X2](custom)) }
  object Mine2 { implicit val mine: Predef[X2] = Predef(Json.schema[X2])           }

  case class X3(a: Int)
  trait Low3 { implicit def low: Schema[X3] = `object`[X3](custom) }
  object Mine3 extends Low3 { implicit val mine: Schema[X3] = Json.schema[X3] }

  case class X5(a: Int)
  object Other5 { implicit val obj: Schema.`object`[X5] = `object`[X5](custom) }
  object Mine5  {
    import Other5._

    implicit val mine: Schema[X5] = Json.schema[X5]
  }

  case class X6(a: Int)
  object Mine6 {
    implicit val obj: Schema.`object`[X6] = `object`[X6](custom)

    implicit val mine: Schema[X6] = Json.schema[X6]
  }

  // the Scala 3 derivation never looks at Json.auto (it is an unused import there)
  case class A1(a: Int)
  @nowarn("msg=unused import")
  object WithAuto {
    import Json.auto._
    implicit val p: Predef[A1] = Predef(Json.schema[A1])
  }

  case class A2(a: Int)
  @nowarn("msg=unused import")
  object A2 {
    import Json.auto._
    implicit val p: Predef[A2] = Predef(Json.schema[A2])
  }

  case class Q(a: Int)
  object TwoLineWithPredef {
    implicit val pp: Predef[Q] = Predef(`string`.asInstanceOf[Schema[Q]])
    implicit val s: Schema[Q]  = Json.schema[Q]
  }
}
