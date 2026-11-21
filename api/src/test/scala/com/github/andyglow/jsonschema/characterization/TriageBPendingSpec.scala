package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, TriageB}
import com.github.andyglow.jsonschema.harness.CompileErrors.outcome
import json._
import json.Schema.{`not` => _, _}
import json.Schema.`object`.Field
import json.schema._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object TriageBPendingSpec {
  case class Empty()
  case class GenericDefault[T](xs: List[T] = Nil)
  case class GenericValueClass[T](v: T) extends AnyVal
  sealed trait Swapped[A, B]
  case class SwappedImpl[B, A](a: A, b: B) extends Swapped[A, B]

  case class RecursiveViaAlias(children: List[RecAlias])
  type RecAlias = RecursiveViaAlias

  case class UuidKeys(m: Map[java.util.UUID, Int])
  case class MutableMap(m: scala.collection.mutable.Map[String, Int])
  sealed trait Key
  object Key {
    case object A extends Key
    case object B extends Key
  }
  case class EnumKeys(m: Map[Key, Int])

  case class User(name: String)
  type Users = List[User]
  case class HasBitSet(b: scala.collection.immutable.BitSet)
  case class Foo(a: Int)
  object Plain       { val foo: Schema[Foo] = Json.schema[Foo]    }
  object FooImplicit { implicit val fooS: Schema[Foo] = Plain.foo }
  case class Recursive(next: Option[Recursive])

  case class Backticked(@description("d") `my-field`: String, @readOnly `+`: Int)

  object Defs { val opt: Option[Int] = Some(1) }
  case class DefaultFromVal(a: Option[Int] = Defs.opt)

  case class HasStale(s: Stale)

  /** @param zz
    *   no such field
    */
  case class Stale(a: Int)

  /** @param a
    *   from scaladoc
    */
  @description("type annotation")
  case class OnlyParams(a: Int)
}

/** TRIAGE group B: each test asserts the FIXED behavior and is pending until the fix lands (then it fails with "should no longer be pending": remove the `pendingUntilFixed` wrapper). Today's Scala 2
  * behavior is in the comments.
  */
class TriageBPendingSpec extends AnyFunSuite {
  import TriageBPendingSpec._

  import ErrorPhrases._

  /** Generic markers of a crash or of an error in the generated code (Scala 2 and Scala 3 wording). */
  private val notMacroOwned = List(
    "exception",
    "matcherror",
    "type mismatch",
    "takes two type parameters",
    "takes 2 type parameters",
    "cannot be applied",
    "not found: ",
    "is not a member of",
    "could not find implicit value",
    "found:",
    "required:",
    "wrong number of type arguments",
    "no given instance"
  )

  /** Fails with a macro-owned error: it contains every one of `phrases` (allowlist) and no crash marker (denylist). */
  private def failsWith(r: Option[String], phrases: String*): Unit = {
    r shouldBe defined
    compilesOrFailsWith(r, phrases: _*)
  }

  /** Compiles, or fails as in [[failsWith]]. */
  private def compilesOrFailsWith(r: Option[String], phrases: String*): Unit = r foreach { m =>
    phrases foreach { p => m should include(p) }
    notMacroOwned foreach { x => m.toLowerCase should not include x }
  }

  private def refsAndDefs(s: Schema[_]): (Set[String], Set[String]) = {
    def go(s: Schema[_]): List[(Option[String], Option[String])] = s match {
      case `ref`(sig)       => List((Some(sig), None))
      case `def`(sig, t)    => (None, Some(sig)) :: go(t)
      case o: `object`[_]   => o.fields.flatMap(f => go(f.tpe))
      case `array`(t, _)    => go(t)
      case `dictionary`(t)  => go(t)
      case `oneof`(ts, _)   => ts.toList.flatMap(go)
      case `allof`(ts)      => ts.toList.flatMap(go)
      case Schema.`not`(t)  => go(t)
      case `value-class`(t) => go(t)
      case `enum`(t, _)     => go(t)
      case _                => Nil
    }
    val xs = go(s)
    (xs.flatMap(_._1).toSet, xs.flatMap(_._2).toSet)
  }

  // --- ref names ---

  test("toDefinition renames refs that point at the old signature", TriageB) {
    pendingUntilFixed {
      // today: the inner ref keeps "A" (dangling $ref)
      `def`[Any]("A", `object`[Any](Field("x", `ref`[Any]("A")))).toDefinition("B") shouldEqual
        `def`[Any]("B", `object`[Any](Field("x", `ref`[Any]("B"))))
      ()
    }
  }

  test("toDefinition doesn't rename nested defs of other types", TriageB) {
    pendingUntilFixed {
      // today: every nested def is renamed to the new name ("C" becomes "B"); found while writing the test above
      `def`[Any]("A", `object`[Any](Field("x", `def`[Any]("C", `string`)))).toDefinition("B") shouldEqual
        `def`[Any]("B", `object`[Any](Field("x", `def`[Any]("C", `string`))))
      ()
    }
  }

  test("refs and defs agree on names through a type alias", TriageB) {
    pendingUntilFixed {
      val (refs, defs) = refsAndDefs(Json.schema[RecursiveViaAlias])
      refs should not be empty
      refs.foreach(r => defs should contain(r))
    }
  }

  // --- unsupported map keys ---

  test("Map with a UUID key: clear compile error naming the key type", TriageB) {
    pendingUntilFixed {
      // today: an error in the generated code: `scala.collection.immutable.Map takes two type parameters, expected: one`
      failsWith(outcome(assertCompiles("Json.schema[UuidKeys]")), ClearUnsupported, "java.util.UUID")
    }
  }

  test("mutable.Map: clear compile error", TriageB) {
    pendingUntilFixed {
      // today: `scala.collection.mutable.Map takes two type parameters, expected: one`
      failsWith(outcome(assertCompiles("Json.schema[MutableMap]")), ClearUnsupported, "mutable.Map")
    }
  }

  test("enum Map key with EnumsAsOneOf: clear compile error naming the key type", TriageB) {
    pendingUntilFixed {
      // today: `scala.collection.immutable.Map takes two type parameters, expected: one`
      // "TriageBPendingSpec.Key" is not a substring of "...TriageBPendingSpec.EnumKeys"
      failsWith(outcome(assertCompiles("{ implicit val f: Flag with Flag.EnumsAsOneOf = null; Json.schema[EnumKeys] }")), ClearUnsupported, "TriageBPendingSpec.Key")
    }
  }

  // --- crashes -> clear compile errors ---

  test("a parameterless collection alias as root doesn't crash", TriageB) {
    pendingUntilFixed {
      // today: NoSuchElementException: head of empty list (typeArgs.head)
      compilesOrFailsWith(outcome(assertCompiles("Json.schema[Users]")), ClearUnsupported)
    }
  }

  test("a non-generic Iterable (BitSet) doesn't crash", TriageB) {
    pendingUntilFixed {
      // today: NoSuchElementException: head of empty list
      compilesOrFailsWith(outcome(assertCompiles("Json.schema[HasBitSet]")), ClearUnsupported, "BitSet")
      compilesOrFailsWith(outcome(assertCompiles("Json.schema[scala.collection.immutable.BitSet]")), ClearUnsupported, "BitSet")
    }
  }

  test("signature of refined/compound/annotated types doesn't crash", TriageB) {
    pendingUntilFixed {
      // today: MatchError in USignatures
      compilesOrFailsWith(outcome(assertCompiles("Json.sig[AnyRef { def x: Int }]")), ClearUnsupported)
      compilesOrFailsWith(outcome(assertCompiles("Json.sig[Foo with Serializable]")), ClearUnsupported)
      compilesOrFailsWith(outcome(assertCompiles("Json.sig[Int @unchecked]")), ClearUnsupported)
      compilesOrFailsWith(outcome(assertCompiles("{ implicit val s: Schema[Foo with Serializable] = null; Json.schema[Foo with Serializable] }")), ClearUnsupported)
    }
  }

  test("objectSchema with an unrecognized decoration shape doesn't crash", TriageB) {
    pendingUntilFixed {
      // today: MatchError in UFieldDecorations.fromSpec
      failsWith(outcome(assertCompiles("Json.objectSchema[Foo](Seq(\"a\" -> \"d\"): _*)")), ConstantStrings)
    }
  }

  test("objectSchema on a root that resolves to a def: clear compile error", TriageB) {
    pendingUntilFixed {
      // today: type mismatch; found: json.Schema.def[Foo], required: json.Schema.object[Foo]
      failsWith(outcome(assertCompiles("{ import FooImplicit._; Json.objectSchema[Foo]() }")), ObjectSchema)
      failsWith(outcome(assertCompiles("Json.objectSchema[Recursive]()")), ObjectSchema)
    }
  }

  test("an Option default that can't be evaluated at compile time: no macro crash", TriageB) {
    pendingUntilFixed {
      // today: java.lang.Exception: Unable to check isNone for ... May be try non-full rebuild
      // Scala 3 (runtime default getters) compiles it; a Scala 2 fix may instead fail with a clear message about the default
      compilesOrFailsWith(outcome(assertCompiles("Json.schema[DefaultFromVal]")), "default")
    }
  }

  // --- case classes and generics ---

  test("a zero-field case class gives an empty object schema", TriageB) {
    pendingUntilFixed {
      // today: overloaded method value apply ... cannot be applied to () (the generated `object`[T]())
      // TODO(fixer): this can only assert "compiles" while it fails to compile on Scala 2. Once fixed, replace the body with
      //   Json.schema[Empty] shouldEqual `object`.fromList[Empty](Nil)
      assertCompiles("Json.schema[Empty]")
    }
  }

  test("a generic default `xs: List[T] = Nil`", TriageB) {
    pendingUntilFixed {
      // today: Can't infer json value for default value of `...GenericDefault[Int](..., xs: List[T] = Nil)` (T not substituted)
      // TODO(fixer): once fixed, replace the body with
      //   Json.schema[GenericDefault[Int]] shouldEqual `object`[GenericDefault[Int]](
      //     Field.fromJson("xs", `array`[Int, List](`integer`), required = false, default = Some(com.github.andyglow.json.Value.arr())))
      assertCompiles("Json.schema[GenericDefault[Int]]")
    }
  }

  test("a generic value class", TriageB) {
    pendingUntilFixed {
      // today: schema for T is not supported
      // TODO(fixer): once fixed, replace the body with
      //   Json.schema[GenericValueClass[Int]] shouldEqual `value-class`[GenericValueClass[Int], Int](`integer`)
      assertCompiles("Json.schema[GenericValueClass[Int]]")
    }
  }

  test("generic subclasses are substituted by name, not by position", TriageB) {
    pendingUntilFixed {
      // `SwappedImpl[B, A] extends Swapped[A, B]`, so for Swapped[Int, String]: a = Int, b = String
      val s = Json.schema[Swapped[Int, String]]
      s shouldEqual `oneof`.of[Swapped[Int, String]](`object`(Field("a", `integer`), Field("b", `string`)))
      ()
    }
  }

  // --- annotations and scaladoc ---

  test("annotations on backticked/symbolic field names are kept", TriageB) {
    pendingUntilFixed {
      val s = Json.objectSchema[Backticked]()
      s.fields.find(_.name == "my-field").flatMap(_.description) shouldBe Some("d")
      s.fields.find(_.name == "+").map(_.rwMode) shouldBe Some(`object`.Field.RWMode.ReadOnly)
      ()
    }
  }

  test("a scaladoc with only @params doesn't override @description", TriageB) {
    pendingUntilFixed {
      // today: description = Some("")
      Json.schema[OnlyParams].description shouldBe Some("type annotation")
      ()
    }
  }

  test("a stale @param is treated the same on the root and on nested types", TriageB) {
    pendingUntilFixed {
      // today: the root fails (`unknown field: zz`), a nested type is silently accepted. Proposal: a warning everywhere.
      val root   = outcome(assertCompiles("Json.schema[Stale]")).isEmpty
      val nested = outcome(assertCompiles("Json.schema[HasStale]")).isEmpty
      root shouldBe nested
    }
  }
}
