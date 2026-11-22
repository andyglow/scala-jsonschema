package com.github.andyglow.jsonschema.refined

import com.github.andyglow.jsonschema.SchemaOps // `stringify`: Scala 3 doesn't see the enclosing package object's members
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

import scala.io.Source

/** Refined types as case-class fields, sum members (`@definition`, `@discriminator`) and `Option` elements: `FsExample` (one copy per Scala version, `W.` on 2.12, literal types on 2.13+).
  * `refined/fs-example.draft07.json` is the Scala 2 output (2.12 and 2.13 agree).
  */
class RefinedFieldsSpec extends AnyFunSuite {
  import FsExampleJsonSchema._

  private def resource(name: String): String = {
    val src = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name), "UTF-8")
    try src.mkString.replace("\r\n", "\n").trim
    finally src.close()
  }

  test("FsExample, draft 07") {
    fsSchema.stringify(Version.Draft07(id = "http://models.org/example.json")) shouldBe resource("refined/fs-example.draft07.json")
  }
}
