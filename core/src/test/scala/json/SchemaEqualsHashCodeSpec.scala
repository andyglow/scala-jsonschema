package json

import com.github.andyglow.json.Value._
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.HashSet

/** `equals` must be meaningful (no vacuous equality) and consistent with `hashCode` for every node, otherwise `Set[Schema[_]]` (oneOf/allOf members) and the test matchers silently accept wrong
  * schemas.
  */
class SchemaEqualsHashCodeSpec extends AnyWordSpec {

  private def assertEqualAndSameHash(a: Schema[_], b: Schema[_]): Unit = {
    a shouldEqual b
    b shouldEqual a
    a.hashCode shouldBe b.hashCode
    // HashSet always uses hashCode (a small immutable Set1..Set4 would only use equals)
    HashSet[Schema[_]](a, b) should have size 1
  }

  private def assertNotEqual(a: Schema[_], b: Schema[_]): Unit = {
    (a == b) shouldBe false
    (b == a) shouldBe false
  }

  // more than 4 elements, so the Set is a hash-based one and relies on hashCode
  private def bigSet(xs: Schema[_]*): Set[Schema[_]] = HashSet[Schema[_]](`string`, `array`(`string`), `const`(num(42)), `enum`.of[String](`string`, str("x")), `ref`[Any]("dummy")) ++ xs

  "ref" should {
    "compare by signature" in {
      assertEqualAndSameHash(`ref`[Any]("a"), `ref`[Any]("a"))
      assertNotEqual(`ref`[Any]("a"), `ref`[Any]("b"))
    }
    "not be equal to other node types (in either direction)" in {
      assertNotEqual(`ref`[Any]("a"), `integer`)
      assertNotEqual(`ref`[Any]("a"), `boolean`)
      assertNotEqual(`ref`[Any]("a"), `number`[Double])
      assertNotEqual(`ref`[Any]("a"), `string`)
      assertNotEqual(`ref`[Any]("a"), `def`[Any]("a", `string`))
    }
    "take title and description into account" in {
      assertNotEqual(`ref`[Any]("a"), `ref`[Any]("a").withTitle("t"))
    }
    "be found in hash based sets" in {
      bigSet(`ref`[Any]("a")).contains(`ref`[Any]("a")) shouldBe true
      bigSet(`ref`[Any]("a")).contains(`ref`[Any]("b")) shouldBe false
    }
    "make nested refs meaningful" in {
      assertNotEqual(
        `object`[Any](Field("x", `ref`[Any]("a"))),
        `object`[Any](Field("x", `ref`[Any]("b")))
      )
      assertNotEqual(`array`[Any, List](`ref`[Any]("a")), `array`[Any, List](`ref`[Any]("b")))
      assertNotEqual(`def`[Any]("d", `ref`[Any]("a")), `def`[Any]("d", `ref`[Any]("b")))
    }
  }

  "boolean, integer, number" should {
    "have hashCode consistent with equals for fresh instances" in {
      assertEqualAndSameHash(`boolean`, new `boolean`)
      assertEqualAndSameHash(`integer`, new `integer`)
      assertEqualAndSameHash(`number`[Double], `number`[Int])
      assertEqualAndSameHash(`integer`, `integer`.duplicate())
    }
    "be found in hash based sets" in {
      bigSet(new `integer`).contains(`integer`) shouldBe true
      bigSet(`number`[Double]).contains(`number`[Int]) shouldBe true
      bigSet(new `boolean`).contains(`boolean`) shouldBe true
    }
    "not be equal to each other" in {
      assertNotEqual(`boolean`, `integer`)
      assertNotEqual(`integer`, `number`[Int])
    }
  }

  "object" should {
    val a = Field("a", `string`)
    val b = Field("b", `integer`, required = false)
    "ignore field order, with a consistent hashCode" in {
      assertEqualAndSameHash(`object`[Any](a, b), `object`[Any](b, a))
      bigSet(`object`[Any](a, b)).contains(`object`[Any](b, a)) shouldBe true
    }
    "distinguish a free object from a strict one" in {
      assertNotEqual(`object`[Any](a).free, `object`[Any](a))
      assertNotEqual(`object`.Free[Any](), `object`[Any](a).dropField(_ => true))
      assertEqualAndSameHash(`object`[Any](a).free, `object`[Any](a).free)
    }
    "compare fields by description" in {
      assertNotEqual(`object`[Any](a.withDescription(Some("x"))), `object`[Any](a.withDescription(Some("y"))))
      assertNotEqual(`object`[Any](a.withDescription(Some("x"))), `object`[Any](a))
    }
    "compare fields by type" in {
      assertNotEqual(`object`[Any](Field("a", `string`)), `object`[Any](Field("a", `integer`)))
    }
  }

  "the other nodes" should {
    "have hashCode consistent with equals" in {
      assertEqualAndSameHash(`string`[String](Some(`string`.Format.`email`)), `string`[String](Some(`string`.Format.`email`)))
      assertEqualAndSameHash(`array`[String, List](`string`, unique = true), `array`[String, Set](`string`, unique = true))
      assertEqualAndSameHash(`dictionary`[String, Int, Map](`integer`), `dictionary`[String, Int, Map](new `integer`))
      assertEqualAndSameHash(`enum`.of[String](`string`, str("a"), str("b")), `enum`.of[String](`string`, str("b"), str("a")))
      assertEqualAndSameHash(`oneof`.of[Any](`string`, `integer`), `oneof`.of[Any](new `integer`, `string`))
      assertEqualAndSameHash(`allof`.of[Any](`string`, `integer`), `allof`.of[Any](new `integer`, `string`))
      assertEqualAndSameHash(Schema.`not`[Int](`integer`), Schema.`not`[Int](new `integer`))
      assertEqualAndSameHash(`def`[Any]("x", `integer`), `def`[Any]("x", new `integer`))
      assertEqualAndSameHash(`value-class`[Any, Int](`integer`), `value-class`[Any, Int](new `integer`))
      assertEqualAndSameHash(`const`[Any](str("x")), `const`[Any](str("x")))
      assertEqualAndSameHash(Schema.`null`, new Schema.`null`)
      assertEqualAndSameHash(`any`[Int](), `any`[String]())
      assertEqualAndSameHash(`any`[Int]().withTitle("t"), `any`[Int]().withTitle("t"))
      assertEqualAndSameHash(`option`[Int](`integer`), `option`[Int](new `integer`))
      assertEqualAndSameHash(fam(`integer`), fam(new `integer`))
    }
    "not be vacuously equal" in {
      assertNotEqual(`def`[Any]("x", `integer`), `def`[Any]("y", `integer`))
      assertNotEqual(`const`[Any](str("x")), `const`[Any](str("y")))
      assertNotEqual(`oneof`.of[Any](`string`, `integer`), `oneof`.of[Any](`string`, `boolean`))
      assertNotEqual(`array`[String, List](`string`), `array`[String, List](`string`, unique = true))
      assertNotEqual(`option`[Int](`integer`), `option`[String](`string`))
      assertNotEqual(`option`[Int](`integer`), `integer`)
      assertNotEqual(Schema.`null`, `string`)
      assertNotEqual(`any`[Int](), `any`[Int]().withDescription("d"))
      assertNotEqual(`any`[Any](), `object`.Free[Any]())
      assertNotEqual(`any`[Any](), Schema.`null`)
      assertNotEqual(fam(`integer`), fam(`string`))
      assertNotEqual(fam(`integer`), `family`[Any](List(member(`integer`).copy(name = "B"))))
      assertNotEqual(fam(`integer`), fam(`integer`).copy(enumsAsOneOf = true))
      assertNotEqual(fam(`integer`), fam(`integer`).copy(discriminator = Some(`family`.Discriminator())))
      assertNotEqual(`object`[Any](Field("a", `integer`, required = false)), `object`[Any](Field("a", `integer`, required = false).withHasDefault(true)))
    }
  }

  private def member(s: Schema[_]): `family`.Member = `family`.Member("A", "p.A", "p.A", `family`.Kind.Product, s)
  private def fam(s: Schema[_]): `family`[Any]      = `family`[Any](List(member(s)))
}
