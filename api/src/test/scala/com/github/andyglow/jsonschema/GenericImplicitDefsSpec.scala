package com.github.andyglow.jsonschema

import com.github.andyglow.json.{JsonFormatter, Value}
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.harness.Untriaged
import json.Json
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Version.Draft07
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite

// #297: a generic implicit `def f[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]`. Inside it, `A` is a type parameter: no definition name known at compile time is right for every `A`.
class GenericImplicitDefsSpec extends AnyFunSuite {
  import GenericImplicitDefsSpec._

  private val pkg = "com.github.andyglow.jsonschema.GenericImplicitDefsSpec"
  private val bar = `object`[Bar](Field("x", `integer`))
  private val baz = `object`[Baz](Field("y", `string`))

  test("#297: Foo[Bar] and Foo[Baz] through a generic implicit def are two definitions, each with its own body for A") {
    import Plain._
    Json.schema[Quux] shouldEqual `object`[Quux](
      Field("foobar", `def`[Foo[Bar]](s"$pkg.Foo[$pkg.Bar]", `object`[Foo[Bar]](Field("data", bar)))),
      Field("foobaz", `def`[Foo[Baz]](s"$pkg.Foo[$pkg.Baz]", `object`[Foo[Baz]](Field("data", baz))))
    )
  }

  test("#297: an evidence that is already a definition keeps its own name inside the generic def") {
    import Named._
    Json.schema[Quux] shouldEqual `object`[Quux](
      Field("foobar", `def`[Foo[Bar]](s"$pkg.Foo[$pkg.Bar]", `object`[Foo[Bar]](Field("data", `def`[Bar]("Bar", bar))))),
      Field("foobaz", `def`[Foo[Baz]](s"$pkg.Foo[$pkg.Baz]", `object`[Foo[Baz]](Field("data", `def`[Baz]("Baz", baz)))))
    )
  }

  private def field(s: Schema[_], name: String): Schema[_] = s match {
    case d: `def`[_]    => field(d.tpe, name)
    case o: `object`[_] => o.fields.find(_.name == name).map(_.tpe).getOrElse(fail(s"no field $name in $o"))
    case other          => fail(s"not an object: $other")
  }

  private def sigOf(s: Schema[_]): String = s match {
    case d: `def`[_] => d.sig
    case r: `ref`[_] => r.sig
    case other       => fail(s"not a definition: $other")
  }

  private def definitionNames(v: Value): Set[String] = v match {
    case o: obj => o.value.get("definitions").collect { case d: obj => d.keys.toSet }.getOrElse(Set.empty)
    case _      => Set.empty
  }

  // every $ref and definition name that names a type parameter of this spec's models (`...Plain.A`, `...Members.T`, Scala 3's `...jsonSchemaFoo.A`)
  private def placeholders(v: Value): List[String] = {
    val param                         = """\.[ABTF](_|,|$)""".r
    def names(v: Value): List[String] = v match {
      case o: obj =>
        o.fields.toList.flatMap {
          case (k, str(s)) if k == f"$$ref" => List(s)
          case ("definitions", d: obj)      => d.keys.toList ++ names(d)
          case (_, x)                       => names(x)
        }
      case a: arr => a.value.toList.flatMap(names)
      case _      => Nil
    }
    names(v).filter(n => param.findFirstIn(n).isDefined)
  }

  private def treeDef(sig: String, v: Schema[_]) =
    `def`[Any](sig, `object`[Any](Field("v", v), Field("kids", `array`(`ref`[Any](sig)))))

  test("#297: a recursive Tree[A] through a generic implicit def: Tree[Bar] and Tree[Baz] are two definitions, refs included") {
    import Plain._
    Json.schema[Forest] shouldEqual `object`[Forest](
      Field("a", treeDef(s"$pkg.Tree[$pkg.Bar]", bar)),
      Field("b", treeDef(s"$pkg.Tree[$pkg.Baz]", baz))
    )
  }

  test("#297: a definition a generic implicit def names itself is renamed to the concrete type") {
    import UserNamed._
    Json.schema[Quux] shouldEqual `object`[Quux](
      Field("foobar", `def`[Foo[Bar]](s"$pkg.Foo[$pkg.Bar]", `object`[Foo[Bar]](Field("data", bar)))),
      Field("foobaz", `def`[Foo[Baz]](s"$pkg.Foo[$pkg.Baz]", `object`[Foo[Baz]](Field("data", baz))))
    )
  }

  test("#297: an implicit Schema for an abstract type member is used as it is, not named after the member") {
    BarMembers.fooSchema shouldEqual `object`[Foo[Bar]](Field("data", bar))
  }

  test("#297: an implicit Schema for an existential type is still a definition") {
    import Existential._
    Json.schema[HasAny] match {
      case o: `object`[_] => o.fields.head.tpe shouldBe a[`def`[_]]
      case other          => fail(s"not an object: $other")
    }
  }

  test("#297: draft-07 of the issue's model has no definition named after the type parameter") {
    import Plain._
    val out = AsValue.schema(Json.schema[Quux], Draft07("Quux"))
    definitionNames(out) shouldEqual Set(s"$pkg.Foo_$pkg.Bar_", s"$pkg.Foo_$pkg.Baz_")
    placeholders(out) shouldBe empty
  }

  test("#297: two type parameters, Pair[Bar, Baz] and Pair[Baz, Bar]") {
    import Plain._
    Json.schema[Pairs] shouldEqual `object`[Pairs](
      Field("p", `def`[Pair[Bar, Baz]](s"$pkg.Pair[$pkg.Bar,$pkg.Baz]", `object`[Pair[Bar, Baz]](Field("a", bar), Field("b", baz)))),
      Field("q", `def`[Pair[Baz, Bar]](s"$pkg.Pair[$pkg.Baz,$pkg.Bar]", `object`[Pair[Baz, Bar]](Field("a", baz), Field("b", bar))))
    )
  }

  test("#297: the type parameter inside a List, an Option and a Map") {
    import Plain._
    val s = Json.schema[Manies]
    for ((name, x) <- List("a" -> bar, "b" -> baz)) {
      val many = field(s, name)
      field(many, "l") shouldEqual `array`[Any, List](x.asInstanceOf[Schema[Any]])
      field(many, "o") shouldEqual `option`[Any](x.asInstanceOf[Schema[Any]])
      field(many, "m") shouldEqual `dictionary`[String, Any, Map](x.asInstanceOf[Schema[Any]])
    }
    placeholders(AsValue.schema(s, Draft07("id"))) shouldBe empty
  }

  test("#297: a generic sealed family through a generic implicit def") {
    import Plain._
    val s = Json.schema[Results]
    sigOf(field(s, "a")) shouldEqual s"$pkg.Res[$pkg.Bar]"
    sigOf(field(s, "b")) shouldEqual s"$pkg.Res[$pkg.Baz]"
    val out = AsValue.schema(s, Draft07("id"))
    placeholders(out) shouldBe empty
    JsonFormatter.format(out) should (include("\"x\"") and include("\"y\""))
  }

  test("#297: a higher-kinded type parameter in constructor position (F[Bar]) counts as abstract") {
    import HigherKinded._
    Json.schema[HK] shouldEqual `object`[HK](
      Field("l", `def`[Foo[List[Bar]]](s"$pkg.Foo[scala.List[$pkg.Bar]]", `object`[Foo[List[Bar]]](Field("data", `array`[Bar, List](bar))))),
      Field("s", `def`[Foo[Set[Bar]]](s"$pkg.Foo[scala.Predef.Set[$pkg.Bar]]", `object`[Foo[Set[Bar]]](Field("data", `array`[Bar, Set](bar, unique = true)))))
    )
  }

  test("#297: a generic class's type parameter (class Wrap[T: Schema]) is used as it is") {
    import Plain._
    new FooWrap[Bar].s shouldEqual `object`[Foo[Bar]](Field("data", bar))
  }

  // Shapes the fix leaves invalid, on both versions (README "Known issues"): one definition name, two bodies. Pinned so a later fix shows up here.
  test("#297: a type parameter next to a wildcard (Two[A, _]) counts as abstract") {
    import Wildcard._
    val s = Json.schema[Holders]
    field(field(s, "x"), "t") shouldEqual `object`[Any](Field("a", bar))
    field(field(s, "y"), "t") shouldEqual `object`[Any](Field("a", baz))
  }

  test("#297 known issue: a recursive generic nested in another generic implicit def keeps the type parameter's name", Untriaged) {
    import Plain._
    val s  = Json.schema[Nested]
    val ta = field(field(s, "a"), "t")
    val tb = field(field(s, "b"), "t")
    sigOf(ta) shouldEqual sigOf(tb)
    ta should !==(tb)
  }

  test("#297 known issue: a definition from a generic class's member (no type application at the use site) keeps the type parameter's name", Untriaged) {
    val s = {
      implicit val a: Schema[Tree[Bar]] = TreeWrap.bar.s
      implicit val b: Schema[Tree[Baz]] = TreeWrap.baz.s
      Json.schema[Forest]
    }
    sigOf(field(s, "a")) shouldEqual sigOf(field(s, "b"))
    field(s, "a") should !==(field(s, "b"))
  }
}

object GenericImplicitDefsSpec {
  case class Foo[A](data: A)
  case class Tree[A](v: A, kids: List[Tree[A]])
  case class Bar(x: Int)
  case class Baz(y: String)
  case class Quux(foobar: Foo[Bar], foobaz: Foo[Baz])
  case class Forest(a: Tree[Bar], b: Tree[Baz])

  case class Pair[A, B](a: A, b: B)
  case class Pairs(p: Pair[Bar, Baz], q: Pair[Baz, Bar])
  case class Many[A](l: List[A], o: Option[A], m: Map[String, A])
  case class Manies(a: Many[Bar], b: Many[Baz])
  sealed trait Res[A]
  case class Ok[A](value: A)     extends Res[A]
  case class Err[A](msg: String) extends Res[A]
  case class Results(a: Res[Bar], b: Res[Baz])
  case class Outer[A](t: Tree[A])
  case class Nested(a: Outer[Bar], b: Outer[Baz])
  case class HK(l: Foo[List[Bar]], s: Foo[Set[Bar]])

  object Plain {
    implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]]                 = Json.schema[Foo[A]]
    implicit def jsonSchemaTree[A: Schema]: Schema[Tree[A]]               = Json.schema[Tree[A]]
    implicit def jsonSchemaPair[A: Schema, B: Schema]: Schema[Pair[A, B]] = Json.schema[Pair[A, B]]
    implicit def jsonSchemaMany[A: Schema]: Schema[Many[A]]               = Json.schema[Many[A]]
    implicit def jsonSchemaRes[A: Schema]: Schema[Res[A]]                 = Json.schema[Res[A]]
    implicit def jsonSchemaOuter[A: Schema]: Schema[Outer[A]]             = Json.schema[Outer[A]]
    implicit val jsonSchemaBar: Schema[Bar]                               = Json.schema[Bar]
    implicit val jsonSchemaBaz: Schema[Baz]                               = Json.schema[Baz]
  }

  case class Two[A, B](a: A)
  case class Holder[A](t: Two[A, _])
  case class Holders(x: Holder[Bar], y: Holder[Baz])
  object Wildcard {
    implicit def jsonSchemaTwo[A: Schema]: Schema[Two[A, _]]    = Json.schema[Two[A, Any]].asInstanceOf[Schema[Two[A, _]]]
    implicit def jsonSchemaHolder[A: Schema]: Schema[Holder[A]] = Json.schema[Holder[A]]
    implicit val jsonSchemaBar: Schema[Bar]                     = Json.schema[Bar]
    implicit val jsonSchemaBaz: Schema[Baz]                     = Json.schema[Baz]
  }

  object HigherKinded {
    implicit def jsonSchemaFooF[F[_]](implicit s: Schema[F[Bar]]): Schema[Foo[F[Bar]]] = Json.schema[Foo[F[Bar]]]
    implicit val jsonSchemaBars: Schema[List[Bar]]                                     = Json.schema[List[Bar]]
    implicit val jsonSchemaBarSet: Schema[Set[Bar]]                                    = Json.schema[Set[Bar]]
  }

  class FooWrap[T: Schema] {
    val s: Schema[Foo[T]] = Json.schema[Foo[T]]
  }

  class TreeWrap[T: Schema] {
    implicit val s: Schema[Tree[T]] = Json.schema[Tree[T]]
  }
  object TreeWrap {
    val bar = new TreeWrap[Bar]()(Plain.jsonSchemaBar)
    val baz = new TreeWrap[Baz]()(Plain.jsonSchemaBaz)
  }

  object Named {
    implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]
    implicit val jsonSchemaBar: Schema[Bar]               = Json.schema[Bar].toDefinition("Bar")
    implicit val jsonSchemaBaz: Schema[Baz]               = Json.schema[Baz].toDefinition("Baz")
  }

  object UserNamed {
    implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]].toDefinition("Foo")
    implicit val jsonSchemaBar: Schema[Bar]               = Json.schema[Bar]
    implicit val jsonSchemaBaz: Schema[Baz]               = Json.schema[Baz]
  }

  abstract class Members {
    type T
    implicit def tSchema: Schema[T]

    // (a blank line: Scala 2's source-text self-reference check reads the line above, README "Known issues")
    lazy val fooSchema: Schema[Foo[T]] = Json.schema[Foo[T]]
  }
  object BarMembers extends Members {
    type T = Bar
    implicit def tSchema: Schema[Bar] = Json.schema[Bar]
  }

  case class HasAny(f: Foo[_])
  object Existential {
    implicit val jsonSchemaFooAny: Schema[Foo[_]] = Json.schema[Foo[String]].asInstanceOf[Schema[Foo[_]]]
  }
}
