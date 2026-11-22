package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import com.github.andyglow.jsonschema.harness.ErrorPhrases
import json.Json
import json.Schema
import json.Schema._
import json.Schema.`object`.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// A case class with a default declared inside a class (a path-dependent type). Scala 2 referenced its default getter through a static path
// (`<pkg>.Holder.Inner.apply$default$1`), which failed to compile: `object Holder is not a member of package ...`.
class PathDependentDefaultsSpec extends AnyFunSuite {
  import PathDependentDefaultsSpec._

  private val inner = `object`.fromList[Any](
    List(
      Field.fromJson[Int]("a", `integer`, required = false, default = Some(Value.num(1))),
      Field.fromJson[String]("b", `string`, required = false, default = Some(Value.str("x")))
    )
  )

  test("a default of a class-nested case class, derived inside the class") {
    val h = new Holder
    h.innerSchema shouldEqual inner
    h.outerSchema shouldEqual `object`.fromList[Any](List(Field("inner", inner), Field.fromJson[Int]("n", `integer`, required = false, default = Some(Value.num(7)))))
  }

  test("a default of a class-nested case class, derived through a stable path") {
    val h = new Holder
    Json.schema[h.Inner] shouldEqual inner
  }

  test("a default of a case class nested twice in classes") {
    val h = new Holder
    val d = new h.Deeper
    Json.schema[d.Leaf] shouldEqual `object`.fromList[Any](List(Field.fromJson[Int]("z", `integer`, required = false, default = Some(Value.num(3)))))
    d.leafSchema shouldEqual Json.schema[d.Leaf]
  }

  test("a default of a generic class-nested case class") {
    val h = new Holder
    Json.schema[h.Box[String]] shouldEqual `object`.fromList[Any](List(Field.fromJson[List[String]]("xs", `array`[String, List](`string`), required = false, default = Some(Value.arr()))))
  }

  test("a None default of a class-nested case class") {
    val h = new Holder
    Json.schema[h.WithNone] shouldEqual `object`.fromList[Any](List(Field("o", `option`(`integer`), required = false)))
  }

  test("a default of a case class reached through a type projection is a clear compile error (Scala 3 crashed)") {
    val err = errorOf(assertCompiles("Json.schema[Holder#Inner]"))
    err should include(ErrorPhrases.TypeProjectionDefault)
    err should include("Json.schema[o.Inner]")
  }

  test("a default of a case class in an object nested in a class") {
    val h = new Holder
    Json.schema[h.Nested.Leaf] shouldEqual `object`.fromList[Any](List(Field.fromJson[Int]("q", `integer`, required = false, default = Some(Value.num(4)))))
  }
}

object PathDependentDefaultsSpec {

  class Holder {
    case class Inner(a: Int = 1, b: String = "x")
    case class Outer(inner: Inner, n: Int = 7)
    case class Box[T](xs: List[T] = Nil)
    case class WithNone(o: Option[Int] = None)

    def innerSchema: Schema[Inner] = Json.schema[Inner]
    def outerSchema: Schema[Outer] = Json.schema[Outer]

    class Deeper {
      case class Leaf(z: Int = 3)
      def leafSchema: Schema[Leaf] = Json.schema[Leaf]
    }

    object Nested {
      case class Leaf(q: Int = 4)
    }
  }
}
