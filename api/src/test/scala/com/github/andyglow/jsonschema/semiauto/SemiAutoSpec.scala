package com.github.andyglow.jsonschema.semiauto

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.AsValue
import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.{Flag, Predef}
import json.schema.Version.Draft07
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object SemiAutoSpec {
  final case class Address(city: String)

  sealed trait Pet
  object Pet {
    final case class Dog(name: String) extends Pet
    case object Fish                   extends Pet
  }

  final case class Person(address: Address)
  final case class InOption(address: Option[Address])
  final case class InList(addresses: List[Address])
  final case class InMap(addresses: Map[String, Address])
  final case class InTuple(pair: (Int, Address))
  final case class InEither(either: Either[String, Address])
  final case class Owner(pet: Pet)
  final case class PetInList(pets: List[Pet])
  final case class PetInMap(pets: Map[String, Pet])
  final case class PetInEither(either: Either[Pet, Int])

  // native kinds: no instance needed
  sealed trait Color
  object Color {
    case object Red  extends Color
    case object Blue extends Color
  }
  final case class UserId(value: String) extends AnyVal
  final case class Natives(
    i: Int,
    s: String,
    l: List[Int],
    m: Map[String, Long],
    o: Option[Double],
    t: (Int, String),
    e: Either[String, Int],
    color: Color,
    id: UserId,
    arr: Array[String],
    day: java.time.DayOfWeek,
    keyed: Map[Color, Int]
  )

  // a sealed family at the root: its cases (also as fields of each other) are derived as part of it, recursion to it is a $ref
  sealed trait Shape
  object Shape {
    final case class Circle(r: Double)                       extends Shape
    final case class Group(items: List[Shape], main: Circle) extends Shape
    case object Empty                                        extends Shape
  }
  sealed trait BadFamily
  object BadFamily {
    final case class HasAddress(address: Address) extends BadFamily
  }

  final case class Tree(value: Int, children: List[Tree], parent: Option[Tree])

  // a container instance taking a Predef of its element (as cats' NonEmptyList): with auto, the element comes from the same expansion
  final case class Box[A](a: A)
  object Box {
    implicit def boxPredef[A](implicit p: Predef[A]): Predef[Box[A]] = Predef(`array`[A, List](p.schema).asInstanceOf[Schema[Box[A]]])
  }
  final case class Boxed(box: Box[Address])

  // a generic Schema instance taking a Predef of its parameter (the error's hint): a definition named for the instantiated type (#297), its element from auto
  final case class Wrapper[A](data: A)
  object Wrappers {
    implicit def wrapperSchema[A](implicit p: Predef[A]): Schema[Wrapper[A]] = Json.schema[Wrapper[A]]
  }
  final case class HasWrapper(w: Wrapper[Address])

  // recursion with auto, through collections, Option and an instance taking a Predef
  final case class Node(name: String, children: List[Node])
  final case class Forest(trees: List[Node])
  final case class MapNode(children: Map[String, MapNode])
  final case class ArrNode(children: Array[ArrNode])
  final case class OptNode(next: Option[OptNode])
  final case class BoxNode(box: Box[BoxNode])

  // mutual recursion through explicit instances
  final case class MA(b: Option[MB])
  final case class MB(a: Option[MA])
  object Vals {
    implicit val aS: Schema[MA] = Json.schema[MA]
    implicit val bS: Schema[MB] = Json.schema[MB]
  }
  object Lazies {
    implicit lazy val aS: Schema[MA] = Json.schema[MA]
    implicit lazy val bS: Schema[MB] = Json.schema[MB]
  }

  // today's deep derivation (no flag, no import), to compare with
  object Today {
    val person: Schema[Person]           = Json.schema[Person]
    val inOption: Schema[InOption]       = Json.schema[InOption]
    val inList: Schema[InList]           = Json.schema[InList]
    val inMap: Schema[InMap]             = Json.schema[InMap]
    val inTuple: Schema[InTuple]         = Json.schema[InTuple]
    val inEither: Schema[InEither]       = Json.schema[InEither]
    val owner: Schema[Owner]             = Json.schema[Owner]
    val petInList: Schema[PetInList]     = Json.schema[PetInList]
    val petInMap: Schema[PetInMap]       = Json.schema[PetInMap]
    val petInEither: Schema[PetInEither] = Json.schema[PetInEither]
    val node: Schema[Node]               = Json.schema[Node]
    val forest: Schema[Forest]           = Json.schema[Forest]
    val mapNode: Schema[MapNode]         = Json.schema[MapNode]
    val arrNode: Schema[ArrNode]         = Json.schema[ArrNode]
    val optNode: Schema[OptNode]         = Json.schema[OptNode]
  }

  def definitions(x: Schema[_]): Map[String, Value] = AsValue.schema(x, Draft07("x")).value.get("definitions") match {
    case Some(o: obj) => o.value.toMap
    case _            => Map.empty
  }
}

/** Semi-auto derivation (issue #542) with `Flag.SemiAuto` (the phase-2 error now): `Json.schema[T]` derives `T` itself, every nested case class or sealed family needs an instance, or `import
  * json.Json.auto._`.
  */
class SemiAutoSpec extends AnyFunSuite {
  import SemiAutoSpec._

  implicit val jsonSchemaFlags: Flag with Flag.SemiAuto = null

  private def errorOf(f: => Any): String = intercept[TestFailedException](f).getMessage

  private val pkg = "com.github.andyglow.jsonschema.semiauto.SemiAutoSpec"

  private def checkError(msg: String, missing: String, in: String): Unit = {
    msg should include(s"schema for $pkg.$missing is not supported (in $pkg.$in")
    msg should include("no implicit json.Schema or json.schema.Predef for it")
    msg should include("import json.Json.auto._")
  }

  test("a nested case class without an instance is an error, directly and in Option, List, Map values, tuples and Either") {
    checkError(errorOf(assertCompiles("Json.schema[Person]")), "Address", "Person")
    checkError(errorOf(assertCompiles("Json.schema[InOption]")), "Address", "InOption")
    checkError(errorOf(assertCompiles("Json.schema[InList]")), "Address", "InList")
    checkError(errorOf(assertCompiles("Json.schema[InMap]")), "Address", "InMap")
    checkError(errorOf(assertCompiles("Json.schema[InTuple]")), "Address", "InTuple")
    checkError(errorOf(assertCompiles("Json.schema[InEither]")), "Address", "InEither")
    checkError(errorOf(assertCompiles("Json.objectSchema[Person]()")), "Address", "Person")
  }

  test("a nested sealed family without an instance is an error, directly and in List, Map values and Either") {
    checkError(errorOf(assertCompiles("Json.schema[Owner]")), "Pet", "Owner")
    checkError(errorOf(assertCompiles("Json.schema[PetInList]")), "Pet", "PetInList")
    checkError(errorOf(assertCompiles("Json.schema[PetInMap]")), "Pet", "PetInMap")
    checkError(errorOf(assertCompiles("Json.schema[PetInEither]")), "Pet", "PetInEither")
  }

  test("the error names the outermost type without an instance") {
    final case class Outer(person: Person)
    val msg = errorOf(assertCompiles("Json.schema[Outer]"))
    msg should include(s"schema for $pkg.Person is not supported")
  }

  test("a field of a sealed family's case is resolved: an error without an instance") {
    checkError(errorOf(assertCompiles("Json.schema[BadFamily]")), "Address", "BadFamily")
  }

  test("native kinds need no instance: built-ins, collections, Option, tuples, Either, enums, value classes, Java enums, enum map keys") {
    assertCompiles("Json.schema[Natives]")
    val s = Json.schema[Natives].asInstanceOf[`object`[Natives]]
    s.fields.map(_.name) shouldBe List("i", "s", "l", "m", "o", "t", "e", "color", "id", "arr", "day", "keyed")
    s.fields.find(_.name == "keyed").get.tpe.validations.toList.map(_.json) shouldBe List(str("^(?:Red|Blue)$"))
  }

  test("the root is derived: a case class, and a sealed family with its cases (as fields of each other too) and recursion to it") {
    Json.schema[Address] shouldBe `object`[Address](Field("city", `string`))
    assertCompiles("Json.schema[Pet]")
    assertCompiles("Json.schema[Shape]")
    definitions(Json.schema[Shape]).keySet shouldBe Set(s"$pkg.Shape")
  }

  test("recursion to the root") {
    assertCompiles("Json.schema[Tree]")
    definitions(Json.schema[Tree]).keySet shouldBe Set(s"$pkg.Tree")
  }

  test("an explicit instance makes a nested type a definition") {
    implicit val addressSchema: Schema[Address] = Json.schema[Address]
    Json.schema[Person] shouldBe `object`[Person](Field("address", `def`[Address](s"$pkg.Address", `object`[Address](Field("city", `string`)))))
    Json.schema[InList] shouldBe `object`[InList](Field("addresses", `array`[Address, List](`def`[Address](s"$pkg.Address", `object`[Address](Field("city", `string`))))))
  }

  test("with import Json.auto._ the same nested types compile, with today's output") {
    import Json.auto._
    Json.schema[Person] shouldBe Today.person
    Json.schema[InOption] shouldBe Today.inOption
    Json.schema[InList] shouldBe Today.inList
    Json.schema[InMap] shouldBe Today.inMap
    Json.schema[InTuple] shouldBe Today.inTuple
    Json.schema[InEither] shouldBe Today.inEither
    Json.schema[Owner] shouldBe Today.owner
    Json.schema[PetInList] shouldBe Today.petInList
    Json.schema[PetInMap] shouldBe Today.petInMap
    Json.schema[PetInEither] shouldBe Today.petInEither
  }

  test("auto: an instance that takes a Predef of its element gets it from the same expansion") {
    import Json.auto._
    Json.schema[Boxed] shouldBe `object`[Boxed](Field("box", `array`[Address, List](`object`[Address](Field("city", `string`))).asInstanceOf[Schema[Box[Address]]]))
  }

  test("auto: a generic Schema instance taking a Predef: a definition named for the instantiated type, its element from the same expansion") {
    import Json.auto._
    import Wrappers._
    Json.schema[HasWrapper] shouldBe `object`[HasWrapper](
      Field("w", `def`[Wrapper[Address]](s"$pkg.Wrapper[$pkg.Address]", `object`[Wrapper[Address]](Field("data", `object`[Address](Field("city", `string`))))))
    )
  }

  test("auto: recursion through List, Map, Array and Option gives $refs and terminates, as today") {
    import Json.auto._
    Json.schema[Node] shouldBe Today.node
    Json.schema[Forest] shouldBe Today.forest
    Json.schema[MapNode] shouldBe Today.mapNode
    Json.schema[ArrNode] shouldBe Today.arrNode
    Json.schema[OptNode] shouldBe Today.optNode
    definitions(Json.schema[Node]).keySet shouldBe Set(s"$pkg.Node")
    definitions(Today.forest).isEmpty shouldBe false
    definitions(Json.schema[Forest]).keySet shouldBe definitions(Today.forest).keySet
  }

  test("auto: recursion through an instance that takes a Predef of its element") {
    import Json.auto._
    val s = Json.schema[BoxNode]
    s shouldBe `def`[BoxNode](s"$pkg.BoxNode", `object`[BoxNode](Field("box", `array`[BoxNode, List](`ref`[BoxNode](s"$pkg.BoxNode")).asInstanceOf[Schema[Box[BoxNode]]])))
    definitions(s).keySet shouldBe Set(s"$pkg.BoxNode")
  }

  test("mutual recursion through two implicit vals: both definitions, no null, no loop") {
    val a = Vals.aS
    a.toString shouldNot include("null")
    definitions(a).keySet shouldBe Set(s"$pkg.MA", s"$pkg.MB")
    definitions(Vals.bS).keySet shouldBe Set(s"$pkg.MA", s"$pkg.MB")
  }

  test("mutual recursion through two implicit lazy vals: both definitions, no loop") {
    val a = Lazies.aS
    a.toString shouldNot include("null")
    definitions(a).keySet shouldBe Set(s"$pkg.MA", s"$pkg.MB")
    definitions(Lazies.bS).keySet shouldBe Set(s"$pkg.MA", s"$pkg.MB")
    // equality and hash code are finite (a lazy-ref compares by signature)
    (a == Lazies.aS) shouldBe true
    a.hashCode shouldBe Lazies.aS.hashCode
  }
}
