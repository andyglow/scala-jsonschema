package com.github.andyglow.jsonschema.errors

import com.github.andyglow.jsonschema.harness.CompileErrors._
import com.github.andyglow.jsonschema.harness.Scala
import json._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

object ErrorMessagesSpec {
  case object Lonely
  case class HasLonely(o: Lonely.type)
  case class Outer(h: HasLonely)
  sealed trait Colour
  case object Red  extends Colour
  case object Blue extends Colour
  class Plain(val x: Int)
  case class HasPlain(p: Plain)
  case class HasListPlain(l: List[Plain])
  case class HasMapPlain(m: Map[String, Plain])
  case class HasNull(x: Null)
  case class HasNothing(x: Nothing)
  case class HasNullDefault(x: Null = null)
  case class HasOptNothing(x: Option[Nothing])
  case class HasSb(x: java.lang.StringBuilder)
  sealed trait Shape
  case class Circle(r: Int) extends Shape
  class Odd                 extends Shape
  sealed trait Fam
  object Fam { val anon: Fam = new Fam {} }
  case class HasShape(s: Shape)
  case class HasFam(f: Fam)
  sealed trait Top2
  case class T2(i: Int) extends Top2
  sealed trait Sub2     extends Top2
  object Sub2 { val anon: Sub2 = new Sub2 {} }
  case class HasTop2(t: Top2)
  case class Box[A](a: A)
  sealed trait GFam[A]
  case class GI(i: Int)    extends GFam[Int]
  case class GS(s: String) extends GFam[String]
  case class HasGFam(g: GFam[_])
  case class HasOptWild(o: Option[_])
  case class HasEitherWild(e: Either[String, _])
  case class HasTupleWild(t: (Int, _))
  case class HasListOptWild(l: List[Option[_]])
  case class HasBoxWild(b: Box[_])
  case class HasMapKeyWild(m: Map[_, Int])
  case class HasMapValueWild(m: Map[String, _])
  case class HasListWild(l: List[_])
}

/** Issue #471 / #291 (B11): the derivation's compile-error texts, the same on Scala 2 and 3: `schema for <T> is not supported[ (in <A> :: <B>)][: <reason>]. <hint>`. */
class ErrorMessagesSpec extends AnyFunSuite {
  import ErrorMessagesSpec._

  private val M        = "com.github.andyglow.jsonschema.errors.ErrorMessagesSpec."
  private val Hint     = "Provide a json.schema.Predef or json.Schema for it"
  private val NullR    = "Null has no schema of its own. For a JSON null, provide an implicit json.schema.Predef[Null], e.g. json.schema.Predef(json.Schema.`null`)"
  private val Wildcard = "a wildcard type argument (?) has no schema. Use a concrete type, or provide a json.schema.Predef or json.Schema for it"
  private val Alone = "a singleton type (the type of one object) has no schema of its own; objects are derived only as members of a sealed family. Provide a json.schema.Predef or json.Schema for it"
  private def member(b: String) =
    s"a singleton type (the type of one object) has no schema of its own. Use the type of its sealed family $b instead, or provide a json.schema.Predef or json.Schema for it"

  private def noCrash(err: String): Unit = {
    err should not include "exception during macro expansion"
    err should not include "Exception occurred while executing macro expansion"
  }

  /** Scala 3 reports a wildcard; Scala 2 takes the wildcard as `Any` (2.13 normalizes `List[_]` to `List[Any]`), and the `Any` error is accurate there (a `Predef[Any]` makes it work). */
  private def wildcard(err: String, scala3: String): Unit = {
    noCrash(err)
    if (Scala.is2) err should startWith("schema for scala.Any is not supported")
    else err shouldBe scala3
  }

  test("D1: Null") {
    errorOf(assertCompiles("Json.schema[Null]")) shouldBe s"schema for scala.Null is not supported: $NullR"
    errorOf(assertCompiles("Json.schema[Option[Null]]")) shouldBe s"schema for scala.Null is not supported (in scala.Option[scala.Null]): $NullR"
  }

  test("D2: a field of type Null or Nothing is an error, not a crash") {
    errorOf(assertCompiles("Json.schema[HasNull]")) shouldBe s"schema for scala.Null is not supported (in ${M}HasNull): $NullR"
    errorOf(assertCompiles("Json.schema[HasNothing]")) shouldBe s"No json.Schema[_] for Nothing (in ${M}HasNothing)"
    noCrash(errorOf(assertCompiles("Json.schema[HasNullDefault]")))
    noCrash(outcome(assertCompiles("Json.schema[HasOptNothing]")).getOrElse(""))
  }

  test("D2: an implicit Predef[Null] works (the Null hint)") {
    implicit val np: json.schema.Predef[Null] = json.schema.Predef(json.Schema.`null`)
    Json.schema[HasNull] match {
      case o: Schema.`object`[_] => o.fields.map(f => f.name -> f.tpe).toList shouldBe List("x" -> Schema.`null`)
      case other                 => fail(s"not an object: $other")
    }
    outcome(assertCompiles("Json.schema[Option[Null]]")) shouldBe None
    // through the default-value machinery (`= null`): newly compiling on Scala 2
    outcome(assertCompiles("Json.schema[HasNullDefault]")) shouldBe None
  }

  test("D3/D4: fully qualified names, the path only when there is one, the hint") {
    errorOf(assertCompiles("Json.schema[HasSb]")) shouldBe s"schema for java.lang.StringBuilder is not supported (in ${M}HasSb). $Hint"
    errorOf(assertCompiles("Json.schema[java.lang.StringBuilder]")) shouldBe s"schema for java.lang.StringBuilder is not supported. $Hint"
    errorOf(assertCompiles("Json.schema[Plain]")) shouldBe s"schema for ${M}Plain is not supported. $Hint"
    errorOf(assertCompiles("Json.schema[HasPlain]")) shouldBe s"schema for ${M}Plain is not supported (in ${M}HasPlain). $Hint"
    errorOf(assertCompiles("Json.schema[HasListPlain]")) shouldBe s"schema for ${M}Plain is not supported (in ${M}HasListPlain :: scala.collection.immutable.List[${M}Plain]). $Hint"
    errorOf(assertCompiles("Json.schema[HasMapPlain]")) shouldBe
      s"schema for ${M}Plain is not supported (in ${M}HasMapPlain :: scala.collection.immutable.Map[scala.Predef.String, ${M}Plain]). $Hint"
  }

  test("D5: a sealed family member that isn't a case class or object") {
    errorOf(assertCompiles("Json.schema[Shape]")) shouldBe
      s"schema for ${M}Shape is not supported: only case classes and case objects are supported as members of a sealed family, and ${M}Odd is neither. " +
      s"Make it one, or provide a json.schema.Predef or json.Schema for ${M}Shape"
    // the family as a field: the path stops at the family's owner
    errorOf(assertCompiles("Json.schema[HasShape]")) shouldBe
      s"schema for ${M}Shape is not supported (in ${M}HasShape): only case classes and case objects are supported as members of a sealed family, and ${M}Odd is neither. " +
      s"Make it one, or provide a json.schema.Predef or json.Schema for ${M}Shape"
  }

  test("D6/D7: singleton types (#291)") {
    errorOf(assertCompiles("Json.schema[Lonely.type]")) shouldBe s"schema for ${M}Lonely.type is not supported: $Alone"
    errorOf(assertCompiles("Json.schema[HasLonely]")) shouldBe s"schema for ${M}Lonely.type is not supported (in ${M}HasLonely): $Alone"
    errorOf(assertCompiles("Json.schema[Outer]")) shouldBe s"schema for ${M}Lonely.type is not supported (in ${M}Outer :: ${M}HasLonely): $Alone"
    errorOf(assertCompiles("Json.schema[Red.type]")) shouldBe s"schema for ${M}Red.type is not supported: ${member(s"${M}Colour")}"
    errorOf(assertCompiles("Json.schema[None.type]")) shouldBe s"schema for scala.None.type is not supported: ${member("scala.Option")}"
    errorOf(assertCompiles("Json.schema[List[Lonely.type]]")) shouldBe s"schema for ${M}Lonely.type is not supported (in scala.collection.immutable.List[${M}Lonely.type]): $Alone"
  }

  test("D8/D9: wildcard type arguments") {
    wildcard(errorOf(assertCompiles("Json.schema[HasOptWild]")), s"schema for scala.Option[?] is not supported (in ${M}HasOptWild): $Wildcard")
    wildcard(
      errorOf(assertCompiles("Json.schema[HasEitherWild]")),
      s"schema for scala.util.Right[scala.Predef.String, ?] is not supported (in ${M}HasEitherWild :: scala.util.Either[scala.Predef.String, ?]): $Wildcard"
    )
    wildcard(errorOf(assertCompiles("Json.schema[HasTupleWild]")), s"schema for scala.Tuple2[scala.Int, ?] is not supported (in ${M}HasTupleWild): $Wildcard")
    wildcard(
      errorOf(assertCompiles("Json.schema[HasListOptWild]")),
      s"schema for scala.Option[?] is not supported (in ${M}HasListOptWild :: scala.collection.immutable.List[scala.Option[?]]): $Wildcard"
    )
    wildcard(
      errorOf(assertCompiles("Json.schema[HasMapValueWild]")),
      s"schema for scala.collection.immutable.Map[scala.Predef.String, ?] is not supported (in ${M}HasMapValueWild): $Wildcard"
    )
    wildcard(
      errorOf(assertCompiles("Json.schema[HasListWild]")),
      s"schema for scala.collection.immutable.List[?] is not supported (in ${M}HasListWild): $Wildcard"
    )
    wildcard(errorOf(assertCompiles("Json.schema[Set[_]]")), s"schema for scala.collection.immutable.Set[?] is not supported: $Wildcard")

    // a wildcard key: Scala 2 reports the key type `Any`
    val key = errorOf(assertCompiles("Json.schema[HasMapKeyWild]"))
    noCrash(key)
    if (Scala.is2) key should include("the key type scala.Any is none of these")
    else key shouldBe s"schema for scala.collection.immutable.Map[?, scala.Int] is not supported (in ${M}HasMapKeyWild): $Wildcard"

    // a case class with a wildcard argument: Scala 2 resolves the field as `Any`
    val box = errorOf(assertCompiles("Json.schema[HasBoxWild]"))
    noCrash(box)
    if (Scala.is2) box should startWith("schema for scala.Any is not supported")
    else box shouldBe s"schema for ${M}Box[?] is not supported (in ${M}HasBoxWild): $Wildcard"
  }

  test("D8: a wildcard argument that works today keeps working (a generic sealed family)") {
    outcome(assertCompiles("Json.schema[GFam[_]]")) shouldBe None
    outcome(assertCompiles("Json.schema[HasGFam]")) shouldBe None
  }

  test("D10: an anonymous member of a sealed family") {
    def anon(fam: String, name: String, path: String = "") =
      s"schema for $M$fam is not supported$path: it has a subclass that is local to a block or anonymous (e.g. `new $name {}`), which can't be a member of a derived family. " +
        s"Declare the subclasses at the top level or in an object, or provide an implicit json.Schema for $M$fam"
    errorOf(assertCompiles("Json.schema[Fam]")) shouldBe anon("Fam", "Fam")
    errorOf(assertCompiles("Json.schema[HasFam]")) shouldBe anon("Fam", "Fam", s" (in ${M}HasFam)")
    // in a nested sub-family: the sub-family the anonymous class extends is named, not the root
    errorOf(assertCompiles("Json.schema[HasTop2]")) shouldBe anon("Sub2", "Sub2", s" (in ${M}HasTop2)")
  }
}
