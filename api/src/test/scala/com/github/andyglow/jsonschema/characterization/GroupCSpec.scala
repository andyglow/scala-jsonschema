package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{Golden, TriageC}
import json._
import org.scalatest.funsuite.AnyFunSuite

object GroupCSpec {
  case class Foo(a: Int)

  sealed trait Base
  case class Sub1(x: Int)    extends Base
  case class Sub2(y: String) extends Base
  case class HasBase(b: Base)

  trait Open
  case class OpenImpl(z: Int) extends Open
  case class HasOpen(o: Open)

  case class HasEither(e: Either[Int, String])
  case class HasTuple(t: (Int, String))
  case class HasOptOpt(o: Option[Option[Int]])

  // derived in an object without implicits in scope, so the derivation can't pick up the implicit below
  object Plain {
    val sub1: Schema[Sub1]     = Json.schema[Sub1]
    val impl: Schema[OpenImpl] = Json.schema[OpenImpl]
  }
  object Sub1Implicit     { implicit val sub1S: Schema[Sub1] = Plain.sub1     }
  object OpenImplImplicit { implicit val implS: Schema[OpenImpl] = Plain.impl }
}

/** TRIAGE group C: pins TODAY's Scala 2 output. The owner decides whether it changes (see notes/reports/harness.md §Group C).
  */
class GroupCSpec extends AnyFunSuite {
  import GroupCSpec._
  import Golden.check

  test("top-level Option", TriageC) {
    check("groupC/Option[Foo]", Json.schema[Option[Foo]])
    check("groupC/Option[Int]", Json.schema[Option[Int]])
  }

  test("nested Option", TriageC) {
    check("groupC/List[Option[Foo]]", Json.schema[List[Option[Foo]]])
    check("groupC/HasOptOpt", Json.schema[HasOptOpt])
  }

  test("covariant implicit: Schema[Sub1] in scope, Json.schema[Base]", TriageC) {
    import Sub1Implicit._
    check("groupC/covariant.Base", Json.schema[Base])
  }

  test("covariant implicit: Schema[Sub1] in scope, field of type Base", TriageC) {
    import Sub1Implicit._
    check("groupC/covariant.HasBase", Json.schema[HasBase])
  }

  test("covariant implicit: Schema[OpenImpl] in scope, field of a non-sealed trait type", TriageC) {
    import OpenImplImplicit._
    check("groupC/covariant.HasOpen", Json.schema[HasOpen])
  }

  test("Either", TriageC) {
    check("groupC/Either[Int,String]", Json.schema[Either[Int, String]])
    check("groupC/HasEither", Json.schema[HasEither])
  }

  test("tuples", TriageC) {
    check("groupC/Tuple2[Int,String]", Json.schema[(Int, String)])
    check("groupC/HasTuple", Json.schema[HasTuple])
  }
}
