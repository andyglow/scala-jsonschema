package com.github.andyglow.jsonschema.refined

import com.github.andyglow.jsonschema.SchemaOps // `stringify`: Scala 3 doesn't see the enclosing package object's members
import com.github.andyglow.jsonschema.platform.TestFiles
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

/** Refined types as case-class fields, sum members (`@definition`, `@discriminator`) and `Option` elements: `FsExample` (one copy per Scala version, `W.` on 2.12, literal types on 2.13+).
  * `refined/fs-example.draft07.json` is the Scala 2 output (2.12 and 2.13 agree).
  */
class RefinedFieldsSpec extends AnyFunSuite {
  import FsExampleJsonSchema._

  private def resource(name: String): String = TestFiles.resourceText(getClass, name).replace("\r\n", "\n").trim

  test("FsExample, draft 07") {
    fsSchema.stringify(Version.Draft07(id = "http://models.org/example.json")) shouldBe resource("refined/fs-example.draft07.json")
  }
}
