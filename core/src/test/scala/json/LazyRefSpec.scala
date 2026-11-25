package json

import com.github.andyglow.json.Value
import com.github.andyglow.json.Value._
import com.github.andyglow.jsonschema.AsValue
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Version._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** `lazy-ref`, `Schema.building` and `def.adaptOrLazy` (issue #542): mutual recursion through implicit instances. The objects below are what the macros emit for
  * `implicit val aS: Schema[A] = Json.schema[A]` / `implicit val bS: Schema[B] = Json.schema[B]` with `case class A(b: Option[B]); case class B(a: Option[A])`.
  */
object LazyRefSpec {

  private def emitted(sig: String, field: String, other: => Schema[_], otherSig: String): Schema[Any] =
    Schema.building(sig)(`object`[Any](Field(field, `def`.adaptOrLazy[Any](other, otherSig), required = false)))

  /** `implicit lazy val` (Scala 2) or `given` (Scala 3): evaluating the other one re-enters this one. */
  object Lazy {
    lazy val aS: Schema[Any] = emitted("A", "b", bS, "B")
    lazy val bS: Schema[Any] = emitted("B", "a", aS, "A")
  }

  /** `implicit val`: `bS` is still `null` while `aS` is initialized. */
  object Strict {
    val aS: Schema[Any] = emitted("A", "b", bS, "B")
    val bS: Schema[Any] = emitted("B", "a", aS, "A")
  }

  /** No cycle: today's `def`. */
  object NoCycle {
    val cS: Schema[Any] = `object`[Any](Field("x", `integer`))
    val dS: Schema[Any] = emitted("D", "c", cS, "C")
  }
}

class LazyRefSpec extends AnyFunSuite {
  import LazyRefSpec._

  private def defs(x: Schema[_]): Map[String, obj] = AsValue.schema(x, Draft07("x")).value("definitions").asInstanceOf[obj].value.toMap.map { case (k, v) => k -> v.asInstanceOf[obj] }
  private def prop(o: obj, name: String): Value    = o.value("properties").asInstanceOf[obj].value(name)

  test("no cycle: an available instance is a def, as before") {
    NoCycle.dS shouldBe `object`[Any](Field("c", `def`[Any]("C", NoCycle.cS), required = false))
  }

  test("lazy instances in a cycle: the inner reference is a lazy-ref, the schema renders with both definitions") {
    val a = Lazy.aS
    a.toString should include("def(signature = B")
    a.toString should include("lazy-ref(A)")
    // rendering forces A's body (aS, initialized by now): both are definitions, each refers to the other
    val d = defs(a)
    d.keySet shouldBe Set("A", "B")
    prop(d("B"), "a") shouldBe obj(f"$$ref" -> "#A")
    prop(d("A"), "b") shouldBe obj(f"$$ref" -> "#B")
    defs(Lazy.bS).keySet shouldBe Set("A", "B")
  }

  test("strict vals in a cycle: the not-yet-initialized one is a lazy-ref, no null in the tree") {
    val a = Strict.aS
    a.toString should include("lazy-ref(B)")
    a.toString shouldNot include("null")
    val d = defs(a)
    d.keySet shouldBe Set("A", "B")
    prop(d("B"), "a") shouldBe obj(f"$$ref" -> "#A")
    prop(d("A"), "b") shouldBe obj(f"$$ref" -> "#B")
  }

  test("materialized: idempotent, and a tree without lazy-ref is returned as is") {
    Schema.materialized(NoCycle.dS) should be theSameInstanceAs NoCycle.dS
    val once = Schema.materialized(Lazy.aS)
    Schema.materialized(once) shouldBe once
  }

  test("lazy-ref equality and toString are finite (by signature)") {
    val l1 = `def`.adaptOrLazy[Any](null, "Z")
    val l2 = `def`.adaptOrLazy[Any](null, "Z")
    l1 shouldBe l2
    l1.hashCode shouldBe l2.hashCode
    l1.toString shouldBe "lazy-ref(Z)"
  }

  test("building cleans up after an exception") {
    an[IllegalStateException] should be thrownBy Schema.building[Any]("E")(throw new IllegalStateException("boom"))
    Schema.isBuilding("E") shouldBe false
  }
}
