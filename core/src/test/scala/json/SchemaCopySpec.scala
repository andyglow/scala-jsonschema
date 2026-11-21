package json

import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** Copies of a schema keep what they copy (TRIAGE group B, bugs-1 #5 and #6). */
class SchemaCopySpec extends AnyFunSuite {

  private val free = `object`[Any](Field("a", `integer`)).free

  private def isFree(s: Schema[_]): Boolean = s match {
    case `def`(_, t) => isFree(t)
    case s           => (s: Any).isInstanceOf[`object`.Free]
  }

  test("every copy of a free object stays free") {
    isFree(free) shouldBe true
    isFree(free.withTitle("Bag")) shouldBe true
    isFree(free.withDescription("d")) shouldBe true
    isFree(free.withDiscriminationKey("k")) shouldBe true
    isFree(free.duplicate()) shouldBe true
    isFree(free.withField("b", `string`)) shouldBe true
    isFree(free.dropField(_.name == "a")) shouldBe true
    isFree(free.withFieldsUpdated { case f => f.withDescription(Some("x")) }) shouldBe true
    isFree(free.withExtraFrom(`string`)) shouldBe true
    isFree(free.toDefinition("Bag")) shouldBe true
    isFree(`def`[Any]("Bag", free).toDefinition("Bag2")) shouldBe true
    isFree(`object`.Free[Any]().withTitle("t")) shouldBe true
  }

  test("a strict object stays strict, and strict of a copied free object is strict with the same fields and extras") {
    isFree(`object`[Any](Field("a", `integer`)).withTitle("t")) shouldBe false
    val copied = free.withTitle("Bag")
    copied.fields shouldBe free.fields
    copied.title shouldBe Some("Bag")
    val strict = copied.asInstanceOf[`object`[Any] with `object`.Free].strict
    isFree(strict) shouldBe false
    strict.fields shouldBe free.fields
    strict.title shouldBe Some("Bag")
    free.strict shouldEqual `object`[Any](Field("a", `integer`))
  }

  test("withField on a free object adds the field and keeps the extras") {
    val s = free.withTitle("Bag").withField("b", `string`)
    s.fields.map(_.name) shouldBe List("a", "b")
    s.title shouldBe Some("Bag")
    isFree(s) shouldBe true
  }

  test("def.toDefinition renames only its own definition and refs to it") {
    val person = `def`[Any](
      "Person",
      `object`[Any](
        Field("home", `def`[Any]("Address", `object`[Any](Field("street", `string`)))),
        Field("work", `def`[Any]("Address", `object`[Any](Field("street", `string`)))),
        Field("boss", `ref`[Any]("Person")),
        Field("tags", `array`[Any, List](`ref`[Any]("Person"))),
        Field("byName", `dictionary`[String, Any, Map](`ref`[Any]("Person")))
      )
    ).withTitle("P")
    person.toDefinition("Employee") shouldEqual `def`[Any](
      "Employee",
      `object`[Any](
        Field("home", `def`[Any]("Address", `object`[Any](Field("street", `string`)))),
        Field("work", `def`[Any]("Address", `object`[Any](Field("street", `string`)))),
        Field("boss", `ref`[Any]("Employee")),
        Field("tags", `array`[Any, List](`ref`[Any]("Employee"))),
        Field("byName", `dictionary`[String, Any, Map](`ref`[Any]("Employee")))
      )
    ).withTitle("P")
  }

  test("def.toDefinition follows refs inside oneOf, allOf, not and value-class") {
    val d = `def`[Any](
      "A",
      `oneof`[Any](
        Set(
          `allof`[Any](Set(`ref`[Any]("A"), `string`)),
          Schema.`not`[Any](`ref`[Any]("A")),
          `value-class`[Any, Any](`ref`[Any]("A"))
        ),
        None
      )
    )
    d.toDefinition("B") shouldEqual `def`[Any](
      "B",
      `oneof`[Any](
        Set(
          `allof`[Any](Set(`ref`[Any]("B"), `string`)),
          Schema.`not`[Any](`ref`[Any]("B")),
          `value-class`[Any, Any](`ref`[Any]("B"))
        ),
        None
      )
    )
  }
}
