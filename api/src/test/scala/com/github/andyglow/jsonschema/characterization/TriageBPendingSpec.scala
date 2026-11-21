package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden, TriageB}
import com.github.andyglow.jsonschema.harness.CompileErrors.outcome
import json._
import json.Schema.{`not` => _, _}
import json.Schema.`object`.Field
import json.schema._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.annotation.nowarn

object TriageBPendingSpec {
  case class Empty()
  case class HasEmpty(e: Empty)
  case class GenericDefault[T](xs: List[T] = Nil)
  case class GenericValueClass[T](v: T) extends AnyVal
  case class HasGenericValueClass(id: GenericValueClass[String])
  sealed trait Swapped[A, B]
  case class SwappedImpl[B, A](a: A, b: B) extends Swapped[A, B]
  // a parameter nested in the parent's type arguments
  sealed trait Nested[T]
  case class NestedImpl[T](xs: List[T]) extends Nested[List[T]]

  case class RecursiveViaAlias(children: List[RecAlias])
  type RecAlias = RecursiveViaAlias
  case class Box[T](value: T)
  type IntBox = Box[Int]

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

  case class Meta(createdBy: String)
  object Meta { implicit val schema: Schema[Meta] = Json.schema[Meta] }
  case class Node(name: String, meta: Meta, children: List[Node])

  // covariance: Schema is covariant, so an implicit Schema[Sub1] satisfies a search for Schema[Base]
  sealed trait Base
  case class Sub1(x: Int)    extends Base
  case class Sub2(y: String) extends Base
  case class HasBase(b: Base)
  trait Open
  case class OpenImpl(z: Int) extends Open
  case class HasOpen(o: Open)
  // derived where no implicit is in scope
  object CovariancePlain {
    val sub1: Schema[Sub1]     = Json.schema[Sub1]
    val impl: Schema[OpenImpl] = Json.schema[OpenImpl]
  }
  object Sub1Implicit     { implicit val sub1S: Schema[Sub1] = CovariancePlain.sub1     }
  object OpenImplImplicit { implicit val implS: Schema[OpenImpl] = CovariancePlain.impl }

  case class Backticked(@description("d") `my-field`: String, @readOnly `+`: Int)

  object Defs { val opt: Option[Int] = Some(1) }
  case class DefaultFromVal(a: Option[Int] = Defs.opt)

  case class HasStale(s: Stale)

  /** @param zz
    *   no such field
    */
  @nowarn("msg=has @param for unknown field") // the warning under test
  case class Stale(a: Int)

  /** @param a
    *   from scaladoc
    */
  @description("type annotation")
  case class OnlyParams(a: Int)
}

/** TRIAGE group B: each test asserts the fixed behavior (they were `pendingUntilFixed` until the Scala 2 fixes landed on s3/s2fix). The 0.7 behavior is in the `was:` comments. The name is
  * kept because the model signatures (and so the golden files) contain it. The Scala 3 macro must pass these too.
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
    // was: the inner ref kept "A" (dangling $ref)
    `def`[Any]("A", `object`[Any](Field("x", `ref`[Any]("A")))).toDefinition("B") shouldEqual
      `def`[Any]("B", `object`[Any](Field("x", `ref`[Any]("B"))))
  }

  test("toDefinition doesn't rename nested defs of other types", TriageB) {
    // was: every nested def was renamed to the new name ("C" became "B")
    `def`[Any]("A", `object`[Any](Field("x", `def`[Any]("C", `string`)))).toDefinition("B") shouldEqual
      `def`[Any]("B", `object`[Any](Field("x", `def`[Any]("C", `string`))))
  }

  test("toDefinition of a recursive type with a nested definition (bugs-1 #5)", TriageB) {
    // was: meta became `$ref #Tree`, the Meta definition disappeared and children kept the dangling `#Node`
    val s = Json.schema[Node].toDefinition("Tree")
    val (refs, defs) = refsAndDefs(s)
    refs shouldBe Set("Tree")
    defs shouldBe Set("Tree", "com.github.andyglow.jsonschema.characterization.TriageBPendingSpec.Meta")
    Golden.check("triageB/toDefinition.Tree", s)
  }

  test("refs and defs agree on names through a type alias", TriageB) {
    val (refs, defs) = refsAndDefs(Json.schema[RecursiveViaAlias])
    refs should not be empty
    refs.foreach(r => defs should contain(r))
    Golden.check("triageB/alias.RecursiveViaAlias", Json.schema[RecursiveViaAlias])
    // the alias as the root
    Json.schema[RecAlias] shouldEqual Json.schema[RecursiveViaAlias]
  }

  test("an alias of an applied generic as the root (bugs-1 #4)", TriageB) {
    // was: `schema for T is not supported` (the alias has no type arguments, so T wasn't substituted)
    Json.schema[IntBox] shouldEqual Json.schema[Box[Int]]
    Json.schema[IntBox] shouldEqual `object`[Box[Int]](Field("value", `integer`))
    Json.schema[Users] shouldEqual `array`[User, List](`object`[User](Field("name", `string`)))
  }

  // --- unsupported map keys ---

  test("Map with a UUID key: clear compile error naming the key type", TriageB) {
    // was: an error in the generated code: `scala.collection.immutable.Map takes two type parameters, expected: one`
    failsWith(outcome(assertCompiles("Json.schema[UuidKeys]")), ClearUnsupported, "java.util.UUID")
  }

  test("mutable.Map: clear compile error", TriageB) {
    // was: `scala.collection.mutable.Map takes two type parameters, expected: one`
    failsWith(outcome(assertCompiles("Json.schema[MutableMap]")), ClearUnsupported, "mutable.Map")
  }

  test("enum Map key with EnumsAsOneOf: clear compile error naming the key type", TriageB) {
    // was: `scala.collection.immutable.Map takes two type parameters, expected: one`
    // "TriageBPendingSpec.Key" is not a substring of "...TriageBPendingSpec.EnumKeys"
    failsWith(outcome(assertCompiles("{ implicit val f: Flag with Flag.EnumsAsOneOf = null; Json.schema[EnumKeys] }")), ClearUnsupported, "TriageBPendingSpec.Key")
  }

  // --- crashes -> clear compile errors ---

  test("a parameterless collection alias as root doesn't crash", TriageB) {
    // was: NoSuchElementException: head of empty list (typeArgs.head); fixed by dealiasing (see "an alias of an applied generic as the root")
    outcome(assertCompiles("Json.schema[Users]")) shouldBe None
  }

  test("a non-generic Iterable (BitSet) doesn't crash", TriageB) {
    // was: NoSuchElementException: head of empty list
    compilesOrFailsWith(outcome(assertCompiles("Json.schema[HasBitSet]")), ClearUnsupported, "BitSet")
    compilesOrFailsWith(outcome(assertCompiles("Json.schema[scala.collection.immutable.BitSet]")), ClearUnsupported, "BitSet")
  }

  test("signature of refined/compound/annotated types doesn't crash", TriageB) {
    // was: MatchError in USignatures
    compilesOrFailsWith(outcome(assertCompiles("Json.sig[AnyRef { def x: Int }]")), ClearUnsupported)
    compilesOrFailsWith(outcome(assertCompiles("Json.sig[Foo with Serializable]")), ClearUnsupported)
    compilesOrFailsWith(outcome(assertCompiles("Json.sig[Int @unchecked]")), ClearUnsupported)
    compilesOrFailsWith(outcome(assertCompiles("{ implicit val s: Schema[Foo with Serializable] = null; Json.schema[Foo with Serializable] }")), ClearUnsupported)
  }

  test("objectSchema with an unrecognized decoration shape doesn't crash", TriageB) {
    // was: MatchError in UFieldDecorations.fromSpec
    failsWith(outcome(assertCompiles("Json.objectSchema[Foo](Seq(\"a\" -> \"d\"): _*)")), ConstantStrings)
  }

  test("objectSchema on a root that resolves to a def: clear compile error", TriageB) {
    // was: type mismatch; found: json.Schema.def[Foo], required: json.Schema.object[Foo]
    failsWith(outcome(assertCompiles("{ import FooImplicit._; Json.objectSchema[Foo]() }")), ObjectSchema)
    failsWith(outcome(assertCompiles("Json.objectSchema[Recursive]()")), ObjectSchema)
  }

  test("an Option default that can't be evaluated at compile time: no macro crash", TriageB) {
    // was: java.lang.Exception: Unable to check isNone for ... May be try non-full rebuild
    // Scala 3 (runtime default getters) compiles it; a Scala 2 fix may instead fail with a clear message about the default
    compilesOrFailsWith(outcome(assertCompiles("Json.schema[DefaultFromVal]")), "default")
  }

  // --- case classes and generics ---

  test("a zero-field case class gives an empty object schema", TriageB) {
    // was: overloaded method value apply ... cannot be applied to () (the generated `object`[T]())
    Json.schema[Empty] shouldEqual `object`.fromList[Empty](Nil)
    Json.schema[HasEmpty] shouldEqual `object`[HasEmpty](Field("e", `object`.fromList[Empty](Nil)))
    Golden.check("triageB/HasEmpty", Json.schema[HasEmpty])
  }

  test("a generic default `xs: List[T] = Nil`", TriageB) {
    // was: Can't infer json value for default value of `...GenericDefault[Int](..., xs: List[T] = Nil)` (T not substituted)
    Json.schema[GenericDefault[Int]] shouldEqual `object`[GenericDefault[Int]](
      Field.fromJson("xs", `array`[Int, List](`integer`), required = false, default = Some(com.github.andyglow.json.Value.arr()))
    )
  }

  test("a generic value class", TriageB) {
    // was: schema for T is not supported
    Json.schema[GenericValueClass[Int]] shouldEqual `value-class`[GenericValueClass[Int], Int](`integer`)
    Json.schema[HasGenericValueClass] shouldEqual `object`[HasGenericValueClass](Field("id", `value-class`[GenericValueClass[String], String](`string`)))
  }

  test("generic subclasses are substituted by name, not by position", TriageB) {
    // `SwappedImpl[B, A] extends Swapped[A, B]`, so for Swapped[Int, String]: a = Int, b = String
    val s = Json.schema[Swapped[Int, String]]
    s shouldEqual `oneof`.of[Swapped[Int, String]](`object`(Field("a", `integer`), Field("b", `string`)))
    // NestedImpl[T] extends Nested[List[T]]: for Nested[List[Int]], xs is a List[Int]
    Json.schema[Nested[List[Int]]] shouldEqual `oneof`.of[Nested[List[Int]]](`object`(Field("xs", `array`[Int, List](`integer`))))
  }

  // --- covariant implicit pick (owner decision 2026-11-20: require an implicit whose type matches T exactly) ---

  private def fieldNames(s: Schema[_]): Set[String] = s match {
    case o: `object`[_]   => o.fields.flatMap(f => fieldNames(f.tpe) + f.name).toSet
    case `def`(_, t)      => fieldNames(t)
    case `oneof`(ts, _)   => ts.flatMap(fieldNames)
    case `array`(t, _)    => fieldNames(t)
    case `value-class`(t) => fieldNames(t)
    case _                => Set.empty
  }

  test("an implicit Schema[Sub1] is not used for Json.schema[Base]", TriageB) {
    import Sub1Implicit._
    // was: def("...TriageBPendingSpec.Base", object(x)): the whole family is replaced by Sub1's schema, Sub2 is lost.
    // fixed: Base is derived as its sealed family (Sub1 may come out as a def of the implicit), so both x and y are there
    fieldNames(Json.schema[Base]) shouldBe Set("x", "y")
    Golden.check("triageB/covariant.Base", Json.schema[Base])
    sub1S should not be null
  }

  test("an implicit Schema[Sub1] is not used for a field of type Base", TriageB) {
    import Sub1Implicit._
    // was: field b = def("...Base", object(x))
    fieldNames(Json.schema[HasBase]) shouldBe Set("b", "x", "y")
    Golden.check("triageB/covariant.HasBase", Json.schema[HasBase])
    sub1S should not be null
  }

  test("an implicit Schema[OpenImpl] doesn't make a non-sealed trait derivable", TriageB) {
    // was: compiles, field o = def("...Open", object(z)). fixed: Open has no exact implicit and isn't derivable
    failsWith(outcome(assertCompiles("{ import OpenImplImplicit._; Json.schema[HasOpen] }")), NotSupported, "TriageBPendingSpec.Open")
  }

  // --- annotations and scaladoc ---

  test("annotations on backticked/symbolic field names are kept", TriageB) {
    val s = Json.objectSchema[Backticked]()
    s.fields.find(_.name == "my-field").flatMap(_.description) shouldBe Some("d")
    s.fields.find(_.name == "+").map(_.rwMode) shouldBe Some(`object`.Field.RWMode.ReadOnly)
  }

  test("a scaladoc with only @params doesn't override @description", TriageB) {
    // was: description = Some("")
    Json.schema[OnlyParams].description shouldBe Some("type annotation")
  }

  test("a stale @param is treated the same on the root and on nested types", TriageB) {
    // was: the root fails (`unknown field: zz`), a nested type is silently accepted. Proposal: a warning everywhere.
    val root   = outcome(assertCompiles("Json.schema[Stale]")).isEmpty
    val nested = outcome(assertCompiles("Json.schema[HasStale]")).isEmpty
    root shouldBe nested
  }
}
