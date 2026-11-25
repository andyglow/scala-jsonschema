package com.github.andyglow.jsonschema.semiauto

import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Predef
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object AutoPrecedenceSpec {

  /** A schema auto would never derive: every instance below uses it, so the output shows which instance won. */
  def custom[T]: Schema[T] = `string`[T].withDescription("custom")

  case class InCompanion(a: Int)
  object InCompanion { implicit val predef: Predef[InCompanion] = Predef(custom[InCompanion]) }

  case class SchemaInCompanion(a: Int)
  object SchemaInCompanion { implicit val schema: Schema[SchemaInCompanion] = custom[SchemaInCompanion] }

  case class Generic[T](a: T)
  object Generic { implicit def predef[T]: Predef[Generic[T]] = Predef(custom[Generic[T]]) }

  trait InheritedInstances { implicit val predef: Predef[Inherited] = Predef(custom[Inherited]) }
  case class Inherited(a: Int)
  object Inherited extends InheritedInstances

  case class Local(a: Int)

  case class Imported(a: Int)
  object ImportedInstances { implicit val predef: Predef[Imported] = Predef(custom[Imported]) }

  case class Holder(
    i: Int,
    s: String,
    l: List[Int],
    c: InCompanion,
    sc: SchemaInCompanion,
    g: Generic[Int],
    in: Inherited
  )
  case class HasLocal(l: Local)
  case class HasImported(i: Imported)

  val sigOfSchemaInCompanion: String = "com.github.andyglow.jsonschema.semiauto.AutoPrecedenceSpec.SchemaInCompanion"
}

/** Every instance defined the usual way beats `Json.auto` (issue #542: auto is an `Exported[Predef[T]]`, reached only through the lowest-priority bridge `Predef.importedPredef`), inside a derivation
  * and through `implicitly`.
  */
class AutoPrecedenceSpec extends AnyFunSuite {
  import AutoPrecedenceSpec._

  test("built-ins") {
    import Json.auto._
    implicitly[Predef[Int]].schema shouldEqual `integer`
    implicitly[Predef[String]].schema shouldEqual `string`[String]
    implicitly[Predef[List[Int]]].schema shouldEqual `array`[Int, List](`integer`)
    Json.schema[Int] shouldEqual `integer`
  }

  test("a companion instance, a generic companion instance, an instance inherited by the companion from a parent trait") {
    import Json.auto._
    implicitly[Predef[InCompanion]].schema shouldEqual custom[InCompanion]
    implicitly[Predef[Generic[Int]]].schema shouldEqual custom[Generic[Int]]
    implicitly[Predef[Inherited]].schema shouldEqual custom[Inherited]
    Json.schema[Holder] shouldEqual `object`[Holder](
      Field("i", `integer`),
      Field("s", `string`[String]),
      Field("l", `array`[Int, List](`integer`)),
      Field("c", custom[InCompanion]),
      Field("sc", `def`[SchemaInCompanion](sigOfSchemaInCompanion, custom[SchemaInCompanion])),
      Field("g", custom[Generic[Int]]),
      Field("in", custom[Inherited])
    )
  }

  test("a local instance") {
    import Json.auto._
    implicit val local: Predef[Local] = Predef(custom[Local])
    implicitly[Predef[Local]].schema shouldEqual custom[Local]
    Json.schema[HasLocal] shouldEqual `object`[HasLocal](Field("l", custom[Local]))
  }

  test("an imported instance") {
    import Json.auto._
    import ImportedInstances._
    implicitly[Predef[Imported]].schema shouldEqual custom[Imported]
    Json.schema[HasImported] shouldEqual `object`[HasImported](Field("i", custom[Imported]))
  }
}
