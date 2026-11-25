package com.github.andyglow.jsonschema.semiauto

import com.github.andyglow.jsonschema.AsValue
import json._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.{Exported, Predef, Version}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object AutoSpec {
  case class Foo(a: Int)

  // recursion through containers
  case class Node(name: String, children: List[Node])
  case class Forest(trees: List[Node])
  case class MNode(name: String, children: Map[String, MNode])
  case class ANode(name: String, children: Array[ANode])
  case class ONode(name: String, next: Option[ONode])

  // a container with a `Predef` constructor that needs its element's `Predef`, like cats' `NonEmptyList`: auto fills the element in (a hole, issue #542)
  case class Box[T](items: List[T])
  object Box {
    implicit def boxPredef[T](implicit p: Predef[T]): Predef[Box[T]] = Predef(`array`[T, List](p.schema).asInstanceOf[Schema[Box[T]]])
  }
  case class HasBox(foos: Box[Foo])
  case class BNode(name: String, children: Box[BNode])
  case class BForest(trees: Box[BNode])

  // an element that can't be derived
  class NoSchema
  case class Unsupported(n: NoSchema)
  case class HasBad(xs: List[Unsupported])
  case class HasBadBox(xs: Box[Unsupported])

  // built-in collections: auto-imported output = plain output (design S2-R8)
  case class E(a: Int, b: Option[String])
  sealed trait S
  object S {
    case class S1(e: E) extends S
    case object S2      extends S
  }
  case class Colls(l: List[E], s: Set[E], v: Vector[E], a: Array[E], sm: Map[String, E], im: Map[Int, E], ls: List[S], nested: List[List[E]])

  object Plain {
    val node: Schema[Node]             = Json.schema[Node]
    val forest: Schema[Forest]         = Json.schema[Forest]
    val mnode: Schema[MNode]           = Json.schema[MNode]
    val anode: Schema[ANode]           = Json.schema[ANode]
    val onode: Schema[ONode]           = Json.schema[ONode]
    val colls: Schema[Colls]           = Json.schema[Colls]
    val list: Schema[List[E]]          = Json.schema[List[E]]
    val set: Schema[Set[E]]            = Json.schema[Set[E]]
    val vector: Schema[Vector[E]]      = Json.schema[Vector[E]]
    val array: Schema[Array[E]]        = Json.schema[Array[E]]
    val strMap: Schema[Map[String, E]] = Json.schema[Map[String, E]]
    val intMap: Schema[Map[Int, E]]    = Json.schema[Map[Int, E]]
  }

  object Auto {
    import Json.auto._
    val node: Schema[Node]             = Json.schema[Node]
    val forest: Schema[Forest]         = Json.schema[Forest]
    val mnode: Schema[MNode]           = Json.schema[MNode]
    val anode: Schema[ANode]           = Json.schema[ANode]
    val onode: Schema[ONode]           = Json.schema[ONode]
    val colls: Schema[Colls]           = Json.schema[Colls]
    val list: Schema[List[E]]          = Json.schema[List[E]]
    val set: Schema[Set[E]]            = Json.schema[Set[E]]
    val vector: Schema[Vector[E]]      = Json.schema[Vector[E]]
    val array: Schema[Array[E]]        = Json.schema[Array[E]]
    val strMap: Schema[Map[String, E]] = Json.schema[Map[String, E]]
    val intMap: Schema[Map[Int, E]]    = Json.schema[Map[Int, E]]
    val hasBox: Schema[HasBox]         = Json.schema[HasBox]
    val bnode: Schema[BNode]           = Json.schema[BNode]
    val bforest: Schema[BForest]       = Json.schema[BForest]
  }

  val fooSchema: Schema[Foo] = `object`[Foo](Field("a", `integer`))
}

/** `Json.auto` (issue #542): deep derivation in one expansion, recursion through containers, auto's holes in `Predef` constructors, and the same output as plain derivation for built-in collections.
  */
class AutoSpec extends AnyFunSuite {
  import AutoSpec._

  private val nodeSig  = Json.sig[Node].signature
  private val bnodeSig = Json.sig[BNode].signature

  private def nodeDef(children: Schema[_]): Schema[Node] = `def`[Node](nodeSig, `object`[Node](Field("name", `string`), Field("children", children)))

  private def rendered(s: Schema[_]): String = AsValue.schema(s, Version.Draft07("x")).toString

  test("recursion through List: a definition and a $ref, as without the import") {
    Auto.node shouldEqual nodeDef(`array`[Node, List](`ref`[Node](nodeSig)))
    Auto.node shouldEqual Plain.node
  }

  test("a type that only contains a recursive one (Forest(trees: List[Node])): as without the import, and it renders with a $ref") {
    Auto.forest shouldEqual Plain.forest
    rendered(Auto.forest) should include("$ref")
  }

  test("recursion through Map, Array and Option") {
    Auto.mnode shouldEqual Plain.mnode
    Auto.anode shouldEqual Plain.anode
    Auto.onode shouldEqual Plain.onode
    Auto.mnode.toString should include("ref(")
    Auto.anode.toString should include("ref(")
    Auto.onode.toString should include("ref(")
  }

  test("built-in collections: the same output as without the import (List, Set, Vector, Array, Map[String, _], Map[Int, _])") {
    Auto.list shouldEqual Plain.list
    Auto.set shouldEqual Plain.set
    Auto.vector shouldEqual Plain.vector
    Auto.array shouldEqual Plain.array
    Auto.strMap shouldEqual Plain.strMap
    Auto.intMap shouldEqual Plain.intMap
    Auto.colls shouldEqual Plain.colls
    Auto.list shouldEqual `array`[E, List](Json.schema[E])
  }

  test("a container with a Predef constructor gets its element from auto (an array, not the container's fields)") {
    Auto.hasBox shouldEqual `object`[HasBox](Field("foos", `array`[Foo, List](fooSchema)))
  }

  test("recursion through a container with a Predef constructor: a definition and a $ref") {
    Auto.bnode shouldEqual `def`[BNode](bnodeSig, `object`[BNode](Field("name", `string`), Field("children", `array`[BNode, List](`ref`[BNode](bnodeSig)))))
    // a type that only contains the recursive one: Box[BNode] is an array (no `items` field of Box's own), and it renders with a $ref
    Auto.bforest.toString shouldNot include("items:")
    rendered(Auto.bforest) should include("$ref")
  }

  test("an element that can't be derived, inside List and inside a Predef constructor: a compile error that names it") {
    val inList = intercept[TestFailedException](assertCompiles("{ import Json.auto._; Json.schema[HasBad] }")).getMessage
    inList should include("schema for com.github.andyglow.jsonschema.semiauto.AutoSpec.NoSchema is not supported")
    val inBox = intercept[TestFailedException](assertCompiles("{ import Json.auto._; Json.schema[HasBadBox] }")).getMessage
    inBox should include("schema for com.github.andyglow.jsonschema.semiauto.AutoSpec.NoSchema is not supported")
  }

  test("implicitly[Predef[Foo]] through the bridge, outside a derivation") {
    import Json.auto._
    implicitly[Predef[Foo]].schema shouldEqual fooSchema
    implicitly[Exported[Predef[Foo]]].instance.schema shouldEqual fooSchema
    implicitly[Predef[Node]].schema shouldEqual Plain.node
    implicitly[Predef[Box[Foo]]].schema shouldEqual `array`[Foo, List](fooSchema)
    Json.schema[Foo] shouldEqual fooSchema
  }
}
