package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value._
import json.Schema._
import json.Schema.`object`.Field
import json.schema.Version._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** Definition names per draft grammar (TRIAGE bugs-1 #8, owner decision 2026-11-20). */
class DefinitionNamesSpec extends AnyFunSuite {
  import DefinitionNames._

  private val PlainName  = "^[A-Za-z][-A-Za-z0-9.:_]*$".r
  private val Anchor2020 = "^[A-Za-z_][-A-Za-z0-9._]*$".r

  private val sigs = List(
    "com.acme.Foo",
    "com.acme.Box[scala.Int]",
    "scala.Either[scala.Int,java.lang.String]",
    "demo.odd name",
    "demo.odd$u0020name",
    "a/b~c",
    "42",
    "_x",
    "ünï"
  )

  test("each sanitizer gives a name of its draft's grammar") {
    sigs foreach { s =>
      PlainName.pattern.matcher(plainName(s)).matches() shouldBe true
      Anchor2020.pattern.matcher(anchor2020(s)).matches() shouldBe true
      uriFragment(s) should fullyMatch regex "[-A-Za-z0-9._!$&'()*+,;=:@]*"
    }
  }

  test("conforming names are kept as they are") {
    plainName("com.acme.Foo") shouldBe "com.acme.Foo"
    anchor2020("com.acme.Foo") shouldBe "com.acme.Foo"
    uriFragment("com.acme.Foo") shouldBe "com.acme.Foo"
    plainName("given-name:1") shouldBe "given-name:1"
    uriFragment("demo.odd$u0020name") shouldBe "demo.odd$u0020name"
  }

  test("every non-conforming character becomes _") {
    plainName("com.acme.Box[scala.Int]") shouldBe "com.acme.Box_scala.Int_"
    anchor2020("scala.Either[scala.Int,java.lang.String]") shouldBe "scala.Either_scala.Int_java.lang.String_"
    plainName("demo.odd name") shouldBe "demo.odd_name"
    plainName("demo.odd$u0020name") shouldBe "demo.odd_u0020name"
    anchor2020("a:b") shouldBe "a_b"
    plainName("a:b") shouldBe "a:b"
    uriFragment("a/b~c [d]") shouldBe "a_b_c__d_"
    plainName("ünï") shouldBe "T_n_"
  }

  test("a name must start with a letter (06/07/2019-09) or a letter or _ (2020-12)") {
    plainName("42") shouldBe "T42"
    plainName("_x") shouldBe "T_x"
    anchor2020("_x") shouldBe "_x"
    anchor2020("42") shouldBe "_42"
  }

  test("collisions are resolved deterministically, conforming names win") {
    val schema = `object`[Any](
      Field("a", `def`[Any]("x.A[B]", `string`)),
      Field("b", `def`[Any]("x.A_B_", `string`)),
      Field("c", `def`[Any]("x.A(B)", `string`)),
      Field("d", `ref`[Any]("x.A[B]"))
    )
    assign(schema, plainName) shouldBe Map("x.A_B_" -> "x.A_B_", "x.A(B)" -> "x.A_B__2", "x.A[B]" -> "x.A_B__3")
  }

  test("renderers use the sanitized name in definitions keys, ids, anchors and refs") {
    val box    = `def`[Any]("com.acme.Box[scala.Int]", `object`[Any](Field("next", `ref`[Any]("com.acme.Box[scala.Int]"), required = false)))
    val schema = `object`[Any](Field("box", box))

    val d07 = AsValue.schema(schema, Draft07("x"))
    d07.value("definitions").asInstanceOf[obj].value.keySet shouldBe Set("com.acme.Box_scala.Int_")
    d07.value("properties").asInstanceOf[obj].value("box") shouldBe obj(f"$$ref" -> "#com.acme.Box_scala.Int_")
    val def07 = d07.value("definitions").asInstanceOf[obj].value("com.acme.Box_scala.Int_").asInstanceOf[obj]
    def07.value(f"$$id") shouldBe str("#com.acme.Box_scala.Int_")
    def07.value("properties").asInstanceOf[obj].value("next") shouldBe obj(f"$$ref" -> "#com.acme.Box_scala.Int_")

    val d12   = AsValue.schema(schema, Draft12("x"))
    val def12 = d12.value(f"$$defs").asInstanceOf[obj].value("com.acme.Box_scala.Int_").asInstanceOf[obj]
    def12.value(f"$$anchor") shouldBe str("com.acme.Box_scala.Int_")

    val d04 = AsValue.schema(schema, Draft04())
    d04.value("properties").asInstanceOf[obj].value("box") shouldBe obj(f"$$ref" -> "#/definitions/com.acme.Box_scala.Int_")

    // the raw form and the Schema value keep the signature
    box.sig shouldBe "com.acme.Box[scala.Int]"
  }
}
