package json.schema

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.AsValue
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import json.Schema.`family`.{Definition, Discriminator, Kind, Member}
import json.schema.JsonSchemaProfile._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

/** The `family` node lowered by every kind of profile, on hand-built trees (the macros' output is checked in api). Neutral/Legacy must give the 0.7 trees, member order included. */
class FamilyLoweringSpec extends AnyWordSpec {

  // petshop: sealed trait Animal; case class Dog(name: String); case object Fish
  private val dogObj                = `object`[Any](Field("name", `string`[String](None)))
  private def dog: Member           = Member("Dog", "petshop.Dog", "petshop.Dog", Kind.Product, dogObj)
  private def fish: Member          = Member("Fish", "petshop.Fish", "petshop.Fish", Kind.Singleton, `string`[String](None), value = Some(str("Fish")))
  private def single(n: String)     = Member(n, s"p.$n", s"p.$n", Kind.Singleton, `string`[String](None), value = Some(str(n)))
  private def product(n: String)    = Member(n, s"p.$n", s"p.$n", Kind.Product, `object`[Any](Field(n.toLowerCase, `integer`)))
  private val animal: `family`[Any] = `family`[Any](List(dog, fish))

  private val play    = Neutral.withName("play").withSum(SumStyle.Discriminator("_type")).withSingleton(SingletonStyle.AsTagged).withHonorsSchemaAnnotations(false)
  private val circe   = Neutral.withName("circe").withSum(SumStyle.WrapperObject).withSingleton(SingletonStyle.AsTagged).withMemberName(MemberName.SimpleName).withHonorsSchemaAnnotations(false)
  private val upickle = Neutral
    .withName("upickle")
    .withOptionKeyRequired(true)
    .withSum(SumStyle.Discriminator("$type"))
    .withMemberName(MemberName.SimpleName)
    .withHonorsSchemaAnnotations(false)
  private val json4s   = Neutral.withName("json4s").withSingleton(SingletonStyle.AsTagged).withHonorsSchemaAnnotations(false)
  private val profiles = List(Neutral, Legacy, play, circe, upickle, json4s)

  private def members(s: Schema[_]): List[Schema[_]] = s match {
    case `oneof`(xs, _) => xs.toList
    case other          => fail(s"not a oneof: $other")
  }
  private def render(s: Schema[_], p: JsonSchemaProfile): Value = AsValue.schema(s, Version.Draft07("x"))(implicitly, p)
  private def json(s: Schema[_], p: JsonSchemaProfile): String  = com.github.andyglow.json.JsonFormatter.format(render(s, p))
  private def noFamily(s: Schema[_]): Boolean                   = !s.toDebugString.contains("family")

  "Neutral and Legacy" should {
    "give 0.7's untagged oneOf, products first, the singletons' enum last" in {
      val expected = `oneof`[Any](Set(dogObj, `enum`[Any](`string`[String](None), Set[Value](str("Fish")))), None)
      Neutral.lower(animal) shouldEqual expected
      Legacy.lower(animal) shouldEqual expected
      members(Legacy.lower(animal)) shouldEqual expected.subTypes.toList // member order
    }
    "merge singletons with equal value schemas, moving the enum to the end at each merge" in {
      val f   = `family`[Any](List(single("X"), product("A"), single("Y"), product("B")))
      val out = members(Legacy.lower(f))
      // X: [e(X)]; A: [e, A]; Y merges: [A, e(X, Y)]; B: [A, e, B]
      out.map(_.getClass.getSimpleName) shouldEqual List("object", "enum", "object")
      out(1) shouldEqual `enum`[Any](`string`[String](None), Set[Value](str("X"), str("Y")))
    }
    "keep singletons with different value schemas apart (a @typeHint)" in {
      val y = single("Y").copy(schema = `integer`, value = Some(num(1)))
      val f = `family`[Any](List(single("X"), y, product("A")))
      // A comes after both enums: nothing merged after it
      members(Legacy.lower(f)) shouldEqual List(
        `enum`[Any](`string`[String](None), Set[Value](str("X"))),
        `enum`[Any](`integer`, Set[Value](num(1))),
        `object`[Any](Field("a", `integer`))
      )
    }
    "use a singleton's ToValue value" in {
      val f = `family`[Any](List(dog, fish.copy(value = Some(num(7)), schema = `integer`)))
      members(Legacy.lower(f)).last shouldEqual `enum`[Any](`integer`, Set[Value](num(7)))
    }
    "add the member's texts and @discriminatorKey where the member's schema has none" in {
      val m   = dog.copy(title = Some("T"), description = Some("D"), discriminatorKey = Some("dk"))
      val own = dogObj.withTitle("own")
      val out = members(Legacy.lower(`family`[Any](List(m.copy(schema = own)))))
      out.head.title shouldBe Some("own")
      out.head.description shouldBe Some("D")
      out.head.discriminationKey shouldBe Some("dk")
    }
    "leave a Provided member as it is" in {
      val m = dog.copy(kind = Kind.Provided, title = Some("T"), schema = `def`[Any]("petshop.Dog", dogObj))
      members(Legacy.lower(`family`[Any](List(m, product("A"))))).head shouldEqual `def`[Any]("petshop.Dog", dogObj)
    }
    "honor @discriminator: keys from @discriminatorKey, else the signature" in {
      val f   = `family`[Any](List(dog, product("A").copy(discriminatorKey = Some("a"))), discriminator = Some(Discriminator("kind")))
      val out = Legacy.lower(f).asInstanceOf[`oneof`[_]]
      out.discriminationField shouldBe Some("kind")
      out.subTypes.toList.map(_.discriminationKey) shouldEqual List(Some("petshop.Dog"), Some("a"))
    }
    "wrap @definition members: inferred by sig, named by its name, an existing def renamed" in {
      val f = `family`[Any](
        List(
          dog.copy(definition = Some(Definition.Inferred)),
          product("A").copy(definition = Some(Definition.Named("AA"))),
          product("B").copy(definition = Some(Definition.Named("BB")), schema = `def`[Any]("p.B", dogObj).withTitle("t"))
        )
      )
      val out = members(Legacy.lower(f))
      out.collect { case `def`(sig, _) => sig } shouldEqual List("petshop.Dog", "AA", "BB")
      out.last.title shouldBe Some("t")
    }
    "give a const per singleton with Flag.EnumsAsOneOf, with its texts and key" in {
      val f   = `family`[Any](List(dog, fish.copy(title = Some("F"), discriminatorKey = Some("f"))), enumsAsOneOf = true)
      val out = members(Legacy.lower(f))
      out.last shouldEqual `const`[Any](str("Fish")).withTitle("F").withDiscriminationKey("f")
    }
    "give a family of singletons as the enum alone (or the consts), ignoring @discriminator" in {
      val f = `family`[Any](List(single("X"), single("Y")), discriminator = Some(Discriminator()))
      Legacy.lower(f) shouldEqual `enum`[Any](`string`[String](None), Set[Value](str("X"), str("Y")))
      Legacy.lower(f.copy(enumsAsOneOf = true)) shouldEqual `oneof`[Any](Set(`const`[Any](str("X")), `const`[Any](str("Y"))), None)
    }
    "keep a single-member family a oneOf" in {
      Legacy.lower(`family`[Any](List(dog))) shouldEqual `oneof`[Any](Set(dogObj), None)
    }
    "lower nested option and family nodes inside members" in {
      val inner = `family`[Any](List(single("X"), product("A")))
      val m     = dog.copy(schema = `object`[Any](Field("o", `option`[Int](`integer`), required = false), Field("i", inner)))
      noFamily(Neutral.lower(`family`[Any](List(m)))) shouldBe true
      Neutral.lower(`family`[Any](List(m))).toDebugString should include("null")
    }
    "carry the family's own texts" in {
      Legacy.lower(animal.withTitle("Animal")).title shouldBe Some("Animal")
    }
  }

  "library profiles" should {
    "tag with a discriminator field, singletons as tagged objects (play)" in {
      val out = Neutral.lower(animal.withTitle("x")) // sanity: Neutral first
      out.title shouldBe Some("x")
      val p = play.lower(animal).asInstanceOf[`oneof`[_]]
      p.discriminationField shouldBe Some("_type")
      p.subTypes.toList shouldEqual List(dogObj.withDiscriminationKey("petshop.Dog"), `object`.fromList[Any](Nil).withDiscriminationKey("petshop.Fish"))
      val fishJson = json(animal, play).replaceAll("\\s", "")
      fishJson should include(""""properties":{"_type":{"enum":["petshop.Fish"]}},"required":["_type"]""")
    }
    "wrap in an object named after the member (circe)" in {
      members(circe.lower(animal)) shouldEqual List(
        `object`[Any](Field("Dog", dogObj, required = true)),
        `object`[Any](Field("Fish", `object`.fromList[Any](Nil), required = true))
      )
    }
    "write singletons as strings of their names (upickle)" in {
      val p = upickle.lower(animal).asInstanceOf[`oneof`[_]]
      p.discriminationField shouldBe Some("$type")
      p.subTypes.toList shouldEqual List(dogObj.withDiscriminationKey("Dog"), `enum`[Any](`string`[String](None), Set[Value](str("Fish"))))
    }
    "use the member name, not the ToValue value, for string singletons" in {
      val f = `family`[Any](List(dog, fish.copy(value = Some(num(7)), schema = `integer`)))
      members(upickle.lower(f)).last shouldEqual `enum`[Any](`string`[String](None), Set[Value](str("Fish")))
    }
    "leave members untagged, a singleton an empty object (json4s without hints)" in {
      members(json4s.lower(animal)) shouldEqual List(dogObj, `object`.fromList[Any](Nil))
    }
    "ignore @discriminator / @discriminatorKey, keep @title / @description / @definition" in {
      val f = `family`[Any](
        List(dog.copy(discriminatorKey = Some("dk"), title = Some("T"), definition = Some(Definition.Inferred)), fish),
        discriminator = Some(Discriminator("kind"))
      )
      val p = play.lower(f).asInstanceOf[`oneof`[_]]
      p.discriminationField shouldBe Some("_type")
      val d = p.subTypes.head.asInstanceOf[`def`[_]]
      d.sig shouldBe "petshop.Dog"
      d.tpe.discriminationKey shouldBe Some("petshop.Dog")
      d.tpe.title shouldBe Some("T")
    }
    "tag a Provided member's definition body in an inline copy (reviewer11 F1), and fail on one that can't carry the tag" in {
      val m = dog.copy(kind = Kind.Provided, schema = `def`[Any]("petshop.Dog", dogObj))
      members(play.lower(`family`[Any](List(m, fish)))).head shouldEqual dogObj.withDiscriminationKey("petshop.Dog")
      // the tag is in the member the document is checked against: properties name and _type, both required
      val rendered = json(`family`[Any](List(m, fish)), play).replaceAll("\\s", "")
      rendered should include(""""properties":{"name":{"type":"string"},"_type":{"enum":["petshop.Dog"]}},"required":["name","_type"]""")
      // circe wraps it: no tag needed, the definition stays
      members(circe.lower(`family`[Any](List(m, fish)))).head shouldEqual `object`[Any](Field("Dog", `def`[Any]("petshop.Dog", dogObj), required = true))
      val refM = dog.copy(kind = Kind.Provided, schema = `ref`[Any]("petshop.Dog"))
      the[IllegalArgumentException] thrownBy play.lower(`family`[Any](List(refM, fish))) should have message
        "play: the member petshop.Dog of a family tagged by '_type' has an implicit schema that isn't an object (ref(petshop.Dog)), so the tag can't be added to it. " +
        "Give Dog an implicit Schema of an object, or the whole family a hand-written Schema."
    }
    "tag a derived member's definition body (recursive / @definition)" in {
      val m = dog.copy(schema = `def`[Any]("petshop.Dog", dogObj))
      members(play.lower(`family`[Any](List(m, fish)))).head shouldEqual `def`[Any]("petshop.Dog", dogObj.withDiscriminationKey("petshop.Dog"))
    }
    "write a value-class member as the case class under a library profile, unwrapped otherwise (reviewer11 F4)" in {
      val id = Member("Id", "petshop.Id", "petshop.Id", Kind.ValueClass, `value-class`[Any, String](`string`[String](None)), valueField = Some("v"))
      members(upickle.lower(`family`[Any](List(id, fish)))).head shouldEqual `object`[Any](Field("v", `string`[String](None))).withDiscriminationKey("Id")
      members(circe.lower(`family`[Any](List(id, fish)))).head shouldEqual `object`[Any](Field("Id", `object`[Any](Field("v", `string`[String](None))), required = true))
      members(Neutral.lower(`family`[Any](List(id, fish)))).head shouldEqual `value-class`[Any, String](`string`[String](None))
    }
    "give a oneOf of an object and a definition of a string each member its type (reviewer11 F2)" in {
      val o = `oneof`[Any](Set(dogObj, `def`[Any]("s", `string`[String](None))))
      json(o, Neutral).replaceAll("\\s", "") should include(""""oneOf":[{"type":"object"""")
      // the definition first (F2b)
      val d = `oneof`[Any](Set(`def`[Any]("s", `string`[String](None)), dogObj))
      json(d, Neutral).replaceAll("\\s", "") should include(""""type":"object","additionalProperties"""")
    }
    "render a family with a Provided def member, and an allOf with a ref (it used to throw on `def.jsonType`)" in {
      val m = dog.copy(kind = Kind.Provided, schema = `def`[Any]("petshop.Dog", dogObj))
      val f = `family`[Any](List(product("A"), m, product("B")))
      noException should be thrownBy render(f, Neutral)
      noException should be thrownBy render(f, play)
      noException should be thrownBy render(`allof`[Any](Set(`ref`[Any]("r"), dogObj)), Neutral)
    }
  }

  "honorsSchemaAnnotations with a non-default sumStyle (reviewer11 A2)" should {
    val spray = Neutral.withSum(SumStyle.Discriminator("type")).withMemberName(MemberName.SimpleName).withSingleton(SingletonStyle.AsTagged)
    "apply the knob to an unannotated family, @discriminatorKey replacing the member name" in {
      val p = spray.lower(`family`[Any](List(dog.copy(discriminatorKey = Some("dk")), fish))).asInstanceOf[`oneof`[_]]
      p.discriminationField shouldBe Some("type")
      p.subTypes.toList.map(_.discriminationKey) shouldEqual List(Some("dk"), Some("Fish"))
    }
    "let @discriminator win, with 0.7's keys (signature)" in {
      val p = spray.lower(animal.copy(discriminator = Some(Discriminator("kind")))).asInstanceOf[`oneof`[_]]
      p.discriminationField shouldBe Some("kind")
      p.subTypes.toList.map(_.discriminationKey) shouldEqual List(Some("petshop.Dog"), Some("petshop.Fish"))
    }
  }

  "optionKeyRequired" should {
    "make an Option field without a default required, and keep one with a default (= None) optional" in {
      val o = `object`[Any](
        Field("a", `option`[Int](`integer`), required = false),
        Field("b", `option`[Int](`integer`), required = false).withHasDefault(true),
        Field("c", `integer`, required = false).withHasDefault(true)
      )
      upickle.lower(o).asInstanceOf[`object`[_]].fields.map(f => f.name -> f.required) shouldEqual List("a" -> true, "b" -> false, "c" -> false)
      Neutral.lower(o).asInstanceOf[`object`[_]].fields.map(_.required) shouldEqual List(false, false, false)
    }
  }

  "lowering" should {
    "be idempotent for every pair of profiles, and leave no family node" in {
      val tree = `object`[Any](
        Field("animal", animal),
        Field("o", `option`[Any](animal), required = false),
        Field("all", `family`[Any](List(single("X"), single("Y"))))
      )
      for { p <- profiles; q <- profiles } {
        val once = p.lower(tree)
        noFamily(once) shouldBe true
        q.lower(once) shouldEqual once
      }
    }
    "be what renderers do with a raw tree (Neutral)" in {
      render(animal, Neutral) shouldEqual render(Neutral.lower(animal), Legacy)
    }
    "rename refs inside families with toDefinition" in {
      val rec = `def`[Any]("A", `family`[Any](List(dog.copy(schema = `object`[Any](Field("next", `ref`[Any]("A")))))))
      rec.toDefinition[Any]("B").toDebugString should include("ref(B)")
    }
  }

  "builders" should {
    "change one knob and keep every other one" in {
      val base = Neutral
        .withName("n")
        .withOption(OptionStyle.Legacy)
        .withOptionKeyRequired(true)
        .withSum(SumStyle.WrapperObject)
        .withSingleton(SingletonStyle.AsTagged)
        .withMemberName(MemberName.SimpleName)
        .withHonorsSchemaAnnotations(false)
      def knobs(p: JsonSchemaProfile) = (p.name, p.optionStyle, p.optionKeyRequired, p.sumStyle, p.singletonStyle, p.memberName, p.honorsSchemaAnnotations)
      knobs(base) shouldEqual (("n", OptionStyle.Legacy, true, SumStyle.WrapperObject, SingletonStyle.AsTagged, MemberName.SimpleName, false))
      knobs(base.withName("m")) shouldEqual knobs(base).copy(_1 = "m")
      knobs(base.withOption(OptionStyle.Nullable)) shouldEqual knobs(base).copy(_2 = OptionStyle.Nullable)
      knobs(base.withOptionKeyRequired(false)) shouldEqual knobs(base).copy(_3 = false)
      knobs(base.withSum(SumStyle.Untagged)) shouldEqual knobs(base).copy(_4 = SumStyle.Untagged)
      knobs(base.withSingleton(SingletonStyle.AsString)) shouldEqual knobs(base).copy(_5 = SingletonStyle.AsString)
      knobs(base.withMemberName(MemberName.FullName)) shouldEqual knobs(base).copy(_6 = MemberName.FullName)
      knobs(base.withHonorsSchemaAnnotations(true)) shouldEqual knobs(base).copy(_7 = true)
    }
    "keep Neutral's and Legacy's knobs" in {
      for (p <- List(Neutral, Legacy)) {
        (p.optionKeyRequired, p.sumStyle, p.singletonStyle, p.memberName, p.honorsSchemaAnnotations) shouldEqual
          ((false, SumStyle.Untagged, SingletonStyle.AsString, MemberName.FullName, true))
      }
      Neutral.toString shouldBe "JsonSchemaProfile(neutral)"
    }
  }
}
