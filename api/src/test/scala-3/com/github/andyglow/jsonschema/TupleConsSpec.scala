package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.harness.Golden
import json.*
import json.Schema.{`integer`, `object`, `string`}
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scala.compiletime.testing.typeCheckErrors

object TupleConsSpec {
  type T23 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

  case class HasCons(t: Int *: String *: EmptyTuple)
  case class HasTuple(t: (Int, String))

  case class ConcatTail[A <: Tuple, B <: Tuple](t: Int *: Tuple.Concat[A, B])
  case class ConcatField[A <: Tuple, B <: Tuple](t: Tuple.Concat[A, B])
  case class Has3(t: (Int, String, Boolean))
  case class Has2(t: (String, Boolean))

  case class Has23(t: T23)
  object Given23 {
    given Schema[T23] = Json.schema[T23]
  }
}

/** Scala 3 tuple types spelled as `*:` chains (#467, `docs/design/467-s3-tuple-cons.md`). */
class TupleConsSpec extends AnyFunSuite {
  import TupleConsSpec.*

  test("a *: chain of known arity is the TupleN: root, field, collection and Option elements") {
    Json.schema[Int *: String *: EmptyTuple] shouldEqual Json.schema[(Int, String)]
    Json.schema[HasCons] shouldEqual Json.schema[HasTuple]
    Json.schema[List[Int *: String *: EmptyTuple]] shouldEqual Json.schema[List[(Int, String)]]
    Json.schema[Option[Int *: String *: EmptyTuple]] shouldEqual Json.schema[Option[(Int, String)]]
    Json.schema[Int *: Option[String] *: EmptyTuple] shouldEqual Json.schema[(Int, Option[String])]
    Json.schema[Tuple1[Int]] shouldEqual Json.schema[Int *: EmptyTuple]
  }

  test("a *: chain with a TupleN tail, and a reduced Tuple.Concat") {
    Json.schema[Int *: (String, Boolean)] shouldEqual Json.schema[(Int, String, Boolean)]
    Json.schema[Tuple.Concat[Int *: EmptyTuple, String *: EmptyTuple]] shouldEqual Json.schema[(Int, String)]
  }

  test("match-type tails and fields are reduced after type arguments are substituted") {
    Json.schema[ConcatTail[String *: EmptyTuple, Boolean *: EmptyTuple]] shouldEqual Json.schema[Has3]
    Json.schema[ConcatField[String *: EmptyTuple, Boolean *: EmptyTuple]] shouldEqual Json.schema[Has2]
  }

  test("the cons spelling renders exactly like the TupleN (goldens, Neutral and Legacy)") {
    Golden.check("groupC/Tuple2[Int,String]", Json.schema[Int *: String *: EmptyTuple])
    Golden.check("groupC/HasTuple", Json.schema[HasCons])
  }

  test("a root *: chain has no description (not the library's scaladoc of *:)") {
    Json.schema[Int *: String *: EmptyTuple].description shouldBe None
    Json.schema[Int *: String *: EmptyTuple].title shouldBe None
    Json.schema[T23].description shouldBe None
    Json.schema[T23].title shouldBe None
  }

  test("an implicit Schema of the TupleN is a definition for the cons spelling too, named after the TupleN") {
    given Schema[(Int, String)] = `object`[(Int, String)](Field("_1", `integer`), Field("_2", `string`))
    Json.schema[Option[Int *: String *: EmptyTuple]] match {
      case Schema.`option`(d: Schema.`def`[?]) => d.sig shouldBe "scala.Tuple2[scala.Int,scala.Predef.String]"
      case other                               => fail(s"not an option of a definition: $other")
    }
  }

  test("more than 22 elements: the same _1 .. _N object") {
    val fields = (1 to 23).toList.map(i => Field(s"_$i", `integer`))
    Json.schema[T23] shouldEqual `object`.fromList[T23](fields)
    Json.schema[Has23] shouldEqual `object`[Has23](Field("t", `object`.fromList[T23](fields)))
  }

  test("Json.objectSchema takes the cons spelling, also beyond 22 elements (B10-R6)") {
    Json.objectSchema[Int *: String *: EmptyTuple]() shouldEqual Json.objectSchema[(Int, String)]()
    Json.objectSchema[Int *: String *: EmptyTuple]("_1" -> "a number") shouldEqual Json.objectSchema[(Int, String)]("_1" -> "a number")
    Json.objectSchema[T23]() shouldEqual `object`.fromList[T23]((1 to 23).toList.map(i => Field(s"_$i", `integer`)))
    Json.objectSchema[T23]("_23" -> "the last").fields.find(_.name == "_23").flatMap(_.description) shouldBe Some("the last")
    typeCheckErrors("""Json.objectSchema[T23]("_24" -> "none")""").map(_.message).mkString should include("unknown field: _24")
    typeCheckErrors("Json.objectSchema[Tuple]()").map(_.message).mkString should include("can't be used against non-case classes")
  }

  test("more than 22 elements under an implicit: a definition named by the *: signature") {
    import Given23.given
    val name = List.fill(23)("scala.*:[scala.Int,").mkString + "scala.EmptyTuple" + "]" * 23
    Json.schema[Has23] match {
      case o: Schema.`object`[?] =>
        o.fields.toList.map(_.tpe) match {
          case List(d: Schema.`def`[?]) => d.sig shouldBe name
          case other                    => fail(s"not a definition: $other")
        }
      case other => fail(s"not an object: $other")
    }
  }

  test("tuple types of unknown arity and EmptyTuple are compile errors that say why") {
    def errs(es: List[scala.compiletime.testing.Error]) = es.map(_.message).mkString("\n")
    val unknown                                         = List(
      errs(typeCheckErrors("Json.schema[Tuple]")),
      errs(typeCheckErrors("Json.schema[NonEmptyTuple]")),
      errs(typeCheckErrors("Json.schema[Int *: Tuple]")),
      errs(typeCheckErrors("def f[T <: Tuple] = Json.schema[Int *: T]"))
    )
    unknown foreach { e =>
      e should include("a tuple type needs a known arity")
      e should not include "inferring schema for enum"
      e should not endWith ", "
    }
    val empty = errs(typeCheckErrors("Json.schema[EmptyTuple]"))
    empty shouldBe "schema for EmptyTuple is not supported: an empty tuple has no fields"
    errs(typeCheckErrors("Json.schema[Option[Tuple]]")) should include("a tuple type needs a known arity")
  }

  test("tuple types as Map keys get the Map-key error, whatever the spelling") {
    val keys = List(
      typeCheckErrors("Json.schema[Map[Tuple, String]]"),
      typeCheckErrors("Json.schema[Map[NonEmptyTuple, String]]"),
      typeCheckErrors("Json.schema[Map[Int *: EmptyTuple, String]]"),
      typeCheckErrors("Json.schema[Map[(Int, String), String]]")
    ).map(_.map(_.message).mkString("\n"))
    keys foreach { e =>
      e should include("is none of these")
      e should not include "inferring schema for enum"
    }
  }
}
