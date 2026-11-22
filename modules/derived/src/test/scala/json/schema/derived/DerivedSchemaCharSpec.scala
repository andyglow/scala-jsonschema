package json.schema.derived

import json.{Json, Schema}
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers._

object DerivedSchemaCharSpec {
  case class Inner(x: Int)
  case class Outer(i: Inner, s: String)
  case class Unsupported(p: Plain)
  class Plain

  object Plain_        { val inner: Schema[Inner] = Json.schema[Inner]; val outer: Schema[Outer] = Json.schema[Outer] }
  object InnerImplicit { implicit val innerS: Schema[Inner] = Plain_.inner                                            }
  object OuterImplicit { implicit val outerS: Schema[Outer] = Plain_.outer.withTitle("from implicit")                 }

  case class C(a: Int)
  object C      { implicit val d: DerivedSchema[C] = DerivedSchema(Json.schema[C].withTitle("companion")) }
  object LocalD { implicit val d: DerivedSchema[Outer] = DerivedSchema(Plain_.outer.withTitle("local"))   }

  // "not eligible" probes: a default argument, and a lower-priority implicit
  def lookup[T](implicit d: DerivedSchema[T] = null): Option[DerivedSchema[T]] = Option(d)

  trait Via[T] { def via: String }
  trait LowVia {
    implicit def fallback[T]: Via[T] = new Via[T] { def via = "fallback" }
  }
  object Via extends LowVia {
    implicit def derived[T](implicit d: DerivedSchema[T]): Via[T] = new Via[T] { def via = "derived" }
  }
}

/** Characterization of DerivedSchema (inventory E5): the root skips implicit search, nested types don't. */
class DerivedSchemaCharSpec extends AnyFunSuite {
  import DerivedSchemaCharSpec._

  test("the root skips an implicit Schema in scope (derives structurally, no def, no title)") {
    import OuterImplicit._
    implicitly[Schema[Outer]].title shouldBe Some("from implicit")
    implicitly[DerivedSchema[Outer]].schema shouldBe `object`[Outer](
      Field("i", `object`[Inner](Field("x", `integer`))),
      Field("s", `string`)
    )
  }

  test("nested types still use an implicit Schema in scope (as a def)") {
    import InnerImplicit._
    implicitly[DerivedSchema[Outer]].schema shouldBe `object`[Outer](
      Field("i", `def`[Inner]("json.schema.derived.DerivedSchemaCharSpec.Inner", `object`[Inner](Field("x", `integer`)))),
      Field("s", `string`)
    )
  }

  test("Json.auto in scope doesn't get in the way (nested types still use the built-in Predefs)") {
    import json.Json.auto._
    implicitly[DerivedSchema[Outer]].schema shouldBe `object`[Outer](
      Field("i", `object`[Inner](Field("x", `integer`))),
      Field("s", `string`)
    )
    // also uses the import (the derivation never takes auto's given, so on Scala 3 it is otherwise unused)
    implicitly[json.schema.Predef[Inner]].schema shouldBe Plain_.inner
  }

  test("an underivable type makes the implicit unavailable (the search fails, so a default argument applies)") {
    lookup[Unsupported] shouldBe None
    lookup[Inner] shouldBe Some(DerivedSchema(Plain_.inner))
  }

  test("not eligible: an abstract type, a root with no structure (its Predef is skipped like an implicit Schema)") {
    def generic[T]: Option[DerivedSchema[T]] = lookup[T]
    generic[Inner] shouldBe None
    lookup[Int] shouldBe None
  }

  test("a user-provided DerivedSchema wins without ambiguity (companion, import)") {
    implicitly[DerivedSchema[C]].schema.title shouldBe Some("companion")
    import LocalD._
    implicitly[DerivedSchema[Outer]].schema.title shouldBe Some("local")
  }

  test("an underivable type makes the implicit unavailable (a lower-priority implicit is picked instead)") {
    implicitly[Via[Unsupported]].via shouldBe "fallback"
    implicitly[Via[Inner]].via shouldBe "derived"
  }

  test("an underivable type makes the implicit unavailable") {
    val msg = intercept[TestFailedException](assertCompiles("implicitly[DerivedSchema[Unsupported]]")).getMessage
    // the error part only (ScalaTest echoes the code after ", for code:"); Scala 2 / Scala 3 wording
    msg.substring(0, msg.lastIndexOf(", for code:")) should (include("could not find implicit value") or include("No given instance"))
  }
}
