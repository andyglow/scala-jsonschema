package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json._
import json.Schema._
import json.Schema.`family`.{Definition, Discriminator, Kind}
import json.schema.{definition, discriminator, discriminatorKey, title, JsonSchemaProfile}
import json.schema.JsonSchemaProfile._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object FamilyTreeSpec {
  sealed trait Animal
  @title("A dog") final case class Dog(name: String) extends Animal
  @discriminatorKey("fish") case object Fish             extends Animal
  @definition("cat") final case class Cat(lives: Int)   extends Animal

  sealed trait Wrapped extends Any
  final case class Id(v: String)   extends AnyVal with Wrapped
  final case class Other(x: Int)   extends Wrapped

  @discriminator("kind") sealed trait Tagged
  final case class T1(a: Int) extends Tagged
  case object T2              extends Tagged

  sealed trait Color
  case object Red   extends Color
  case object Green extends Color

  sealed trait WithProvided
  final case class Plain(a: Int)     extends WithProvided
  final case class Given(b: String) extends WithProvided
  object Given { implicit val givenSchema: Schema[Given] = `object`[Given](`object`.Field("b", `string`[String](None))).toDefinition("given") }
}

/** The `family` node as both macros derive it (shared: Scala 2 and 3 must agree; Scala 2 lists members in `knownDirectSubclasses` order, so members are compared by name). */
class FamilyTreeSpec extends AnyFunSuite {
  import FamilyTreeSpec._

  private def family(s: Schema[_]): `family`[_] = s match {
    case f: `family`[_]   => f
    case `def`(_, f: `family`[_]) => f
    case other            => fail(s"not a family: $other")
  }
  private def byName(s: Schema[_]) = family(s).members.map(m => m.name -> m).toMap

  test("members: names, full names, signatures, kinds, runtime classes, annotations") {
    val ms = byName(Json.schema[Animal])
    ms.keySet shouldBe Set("Dog", "Fish", "Cat")
    ms("Dog").fullName shouldBe "com.github.andyglow.jsonschema.FamilyTreeSpec.Dog"
    ms("Dog").sig shouldBe "com.github.andyglow.jsonschema.FamilyTreeSpec.Dog"
    ms("Dog").kind shouldBe Kind.Product
    ms("Dog").title shouldBe Some("A dog")
    ms("Dog").cls shouldBe Some(classOf[Dog])
    ms("Dog").schema shouldBe `object`[Dog](`object`.Field("name", `string`[String](None)))
    ms("Fish").kind shouldBe Kind.Singleton
    ms("Fish").value shouldBe Some(str("Fish"))
    ms("Fish").schema shouldBe `string`[String](None)
    ms("Fish").cls shouldBe Some(Fish.getClass)
    ms("Fish").discriminatorKey shouldBe Some("fish")
    ms("Cat").definition shouldBe Some(Definition.Named("cat"))
    family(Json.schema[Animal]).discriminator shouldBe None
  }

  test("a value-class member keeps its field name") {
    val ms = byName(Json.schema[Wrapped])
    ms("Id").kind shouldBe Kind.ValueClass
    ms("Id").valueField shouldBe Some("v")
    ms("Other").kind shouldBe Kind.Product
  }

  test("@discriminator is recorded with its defaults resolved") {
    family(Json.schema[Tagged]).discriminator shouldBe Some(Discriminator("kind", phantom = true))
  }

  test("a family of case objects: singletons only") {
    byName(Json.schema[Color]).values.map(_.kind).toSet shouldBe Set(Kind.Singleton)
  }

  test("an implicit Schema member is Provided, kept as its definition") {
    val ms = byName(Json.schema[WithProvided])
    ms("Given").kind shouldBe Kind.Provided
    ms("Given").schema shouldBe a[`def`[_]]
  }

  test("library knobs on a derived family (play-like: _type, tagged objects)") {
    val play = Neutral.withSum(SumStyle.Discriminator("_type")).withSingleton(SingletonStyle.AsTagged).withHonorsSchemaAnnotations(false)
    val out  = play.lower(Json.schema[Animal]).asInstanceOf[`oneof`[_]]
    out.discriminationField shouldBe Some("_type")
    // @discriminatorKey ignored (R4B), @definition and @title kept
    out.subTypes.collect { case `def`(sig, t) => sig -> t.discriminationKey }.toMap shouldBe Map("cat" -> Some("com.github.andyglow.jsonschema.FamilyTreeSpec.Cat"))
    out.subTypes.flatMap(_.discriminationKey) shouldBe Set("com.github.andyglow.jsonschema.FamilyTreeSpec.Dog", "com.github.andyglow.jsonschema.FamilyTreeSpec.Fish")
    out.subTypes.flatMap(_.title) shouldBe Set("A dog")
  }

  test("Neutral and Legacy shape a derived family the same way") {
    for (s <- List[Schema[_]](Json.schema[Animal], Json.schema[Wrapped], Json.schema[Tagged], Json.schema[Color], Json.schema[WithProvided]))
      JsonSchemaProfile.Neutral.lower(s) shouldBe JsonSchemaProfile.Legacy.lower(s)
  }
}
