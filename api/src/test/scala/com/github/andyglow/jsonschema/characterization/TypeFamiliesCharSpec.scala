package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden, Scala}
import com.github.andyglow.jsonschema.harness.CompileErrors.{errorOf, outcome}
import json._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// models first (SI-7046)
object TypeFamiliesCharSpec {
  case class Shift(day: java.time.DayOfWeek, off: Set[java.time.DayOfWeek], hours: Map[java.time.DayOfWeek, Int], month: Option[java.time.Month])
  case class Curried(a: Int)(val b: String)
  abstract case class Abstract(x: Int)
  case object Lonely
  case class HasLonely(o: Lonely.type)
  class Plain(val x: Int)
  trait JustTrait
  class NonCaseValueClass(val v: Int) extends AnyVal
  case class Box[T](v: T)
  type IntBox = Box[Int]

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

  test("T1: a case class local to a method (bugs-1 #2)") {
    // 0.7: macro crash `<none> is not a module` (Scala 2 reflection gives no companion for a local class)
    case class Local(x: Int, y: Option[String])
    case class HasLocal(l: Local, ls: List[Local])
    check("char/types/Local", Json.schema[Local])
    check("char/types/HasLocal", Json.schema[HasLocal])
  }

  test("T1: a local case class with a default value: a clear error on Scala 2, supported on Scala 3") {
    val r = outcome(assertCompiles("{ case class WithDefault(x: Int = 1); Json.schema[WithDefault] }"))
    if (Scala.is2) r.getOrElse("") should include("default values of a case class local to a block are not supported on Scala 2: WithDefault.x")
    else r shouldBe None
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

  test("T3: a Java enum as a field, in a collection, as a Map key (bugs-1 #3)") {
    // the Java enum on its own: SumsEnumsCharSpec
    check("char/types/Shift", Json.schema[Shift])
  }

  test("T4: a parameterized type alias as root is dealiased (bugs-1 #4)") {
    // `type IntBox = Box[Int]`: 0.7 said `schema for T is not supported` (the alias has no type arguments of its own)
    check("char/types/Box[Int]", Json.schema[Box[Int]])
    check("char/types/Box[Int]", Json.schema[IntBox])
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
    // the flag fully qualified: an import used only inside the quoted code is an unused import on Scala 3
    val err = errorOf(assertCompiles("{ implicit val flag: json.schema.Flag with json.schema.Flag.EnumsAsOneOf = null; Json.schema[EnumKeys] }"))
    // 0.7: an error in the generated code (`array[K, Map]`); now a macro error naming the key type
    err should include(NotSupported)
    err should include("EnumsAsOneOf")
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
