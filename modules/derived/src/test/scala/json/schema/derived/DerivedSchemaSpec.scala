package json.schema.derived

import json.{Json, Schema}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._


class DerivedSchemaSpec extends AnyFunSuite {
  import DerivedSchemaSpec._

  test("compiles") {
    """case class ABC(a: Int, b: String, c: Boolean)
      |val schema = implicitly[DerivedSchema[ABC]]
      |""".stripMargin
  }

  test("equal") {
    implicitly[DerivedSchema[ABC]].schema shouldBe abcSchema
  }

  test("derives") {
    HasSchema[ABC](ABC(1, "2", false)).schema shouldBe abcSchema
  }
}

object DerivedSchemaSpec {

  case class ABC(a: Int, b: String, c: Boolean)

  val abcSchema = Json.schema[ABC]

  case class HasSchema[T](value: T)(implicit val D: DerivedSchema[T]) {
    def schema: Schema[T] = D.schema
  }

}
