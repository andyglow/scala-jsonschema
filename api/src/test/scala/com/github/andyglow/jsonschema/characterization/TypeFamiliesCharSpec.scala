package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden, Scala, Untriaged}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.Schema._
import json.schema.Flag
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// models first (SI-7046)
object TypeFamiliesCharSpec {
  case class Curried(a: Int)(val b: String)
  abstract case class Abstract(x: Int)
  case object Lonely
  case class HasLonely(o: Lonely.type)
  class Plain(val x: Int)
  trait JustTrait
  class NonCaseValueClass(val v: Int) extends AnyVal
  case class Box[T](v: T)
  type IntBox = Box[Int]
  sealed trait NoKids

  object Colors extends Enumeration {
    val Red   = Value("custom-red")
    val Green = Value
    val Blue  = Value(10, "custom-blue")
  }
  case class HasColors(c: Colors.Value)

  case class JavaStringKeys(m: Map[java.lang.String, Int])
  case class ScalaStringKeys(m: Map[String, Int])
  sealed trait Key
  object Key {
    case object B extends Key
    case object A extends Key
  }
  case class EnumKeys(m: Map[Key, Int])
  case class CharKeys(m: Map[Char, Int])
  case class MutableSet(s: scala.collection.mutable.Set[Int])
  case class HasNothing(x: List[Nothing])
}

/** Characterization of type-family derivation on Scala 2 (inventory §3). Successful derivations are pinned by golden files, macro errors by a key phrase from [[ErrorPhrases]]. Compiler-generated
  * texts (crashes, errors in generated code) are checked on Scala 2 only.
  */
class TypeFamiliesCharSpec extends AnyFunSuite {
  import TypeFamiliesCharSpec._
  import Golden.check
  import ErrorPhrases._

  test("T1: a curried case class uses the first parameter list only") {
    check("char/types/Curried", Json.schema[Curried])
  }

  test("T1: a case class local to a method doesn't compile on Scala 2 (crash)", Untriaged) {
    // Scala 3: undecided
    val err = errorOf(assertCompiles("{ case class Local(x: Int, y: Option[String]); Json.schema[Local] }"))
    if (Scala.is2) err should include("is not a module")
  }

  test("T1: an abstract case class doesn't compile (no apply)") {
    errorOf(assertCompiles("Json.schema[Abstract]")) should include(NoApply)
  }

  test("T2: a case object on its own is not supported (root and field)") {
    errorOf(assertCompiles("Json.schema[Lonely.type]")) should include(NotSupported)
    errorOf(assertCompiles("Json.schema[HasLonely]")) should include(NotSupported)
  }

  test("T3: a plain class, a trait and a non-case value class are not supported") {
    errorOf(assertCompiles("Json.schema[Plain]")) should include(NotSupported)
    errorOf(assertCompiles("Json.schema[JustTrait]")) should include(NotSupported)
    errorOf(assertCompiles("Json.schema[NonCaseValueClass]")) should include(NotSupported)
  }

  test("T3: a Java enum doesn't compile on Scala 2 (crash)", Untriaged) {
    // Scala 3: undecided
    val err = errorOf(assertCompiles("Json.schema[java.util.concurrent.TimeUnit]"))
    if (Scala.is2) err should include("ScalaReflectionException")
  }

  test("T4: a parameterized type alias as root is dealiased (bugs-1 #4)") {
    // `type IntBox = Box[Int]`: 0.7 said `schema for T is not supported` (the alias has no type arguments of its own)
    check("char/types/Box[Int]", Json.schema[Box[Int]])
    check("char/types/Box[Int]", Json.schema[IntBox])
  }

  test("T6: a sealed trait without subclasses doesn't compile") {
    errorOf(assertCompiles("Json.schema[NoKids]")) should include(NoItems)
  }

  test("T8: scala.Enumeration uses val names, not the Value(name) strings") {
    val s = Json.schema[Colors.Value]
    check("char/types/Colors.Value", s)
    s shouldBe `enum`.of("Red", "Green", "Blue")
    check("char/types/HasColors", Json.schema[HasColors])
  }

  test("T10: map key patterns") {
    // Map[String, _] has no pattern, Map[java.lang.String, _] gets ^.*$
    check("char/types/ScalaStringKeys", Json.schema[ScalaStringKeys])
    check("char/types/JavaStringKeys", Json.schema[JavaStringKeys])
    // enum keys keep declaration order (B, A)
    check("char/types/EnumKeys", Json.schema[EnumKeys])
    check("char/types/CharKeys", Json.schema[CharKeys])
  }

  test("T10: enum map keys with EnumsAsOneOf don't compile (TRIAGE-B, see TriageBPendingSpec)") {
    val err = errorOf(assertCompiles("{ implicit val flag: Flag with Flag.EnumsAsOneOf = null; Json.schema[EnumKeys] }"))
    // Scala 2: an error in the generated code (`array[K, Map]`)
    if (Scala.is2) err should include regex ("takes (two|2) type parameters")
  }

  test("T11: a mutable.Set is not unique") {
    val s = Json.schema[MutableSet]
    check("char/types/MutableSet", s)
  }

  test("T15: Nothing doesn't compile, also nested") {
    errorOf(assertCompiles("Json.schema[Nothing]")) should include(NoSchemaForNothing)
    errorOf(assertCompiles("Json.schema[HasNothing]")) should include(NoSchemaForNothing)
  }
}
