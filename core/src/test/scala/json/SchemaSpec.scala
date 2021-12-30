package json

import org.scalatest.matchers.should.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.wordspec.AnyWordSpec
import Schema._
import `object`._
import json.schema.validation.Instance._
import org.scalactic.source.Position

class SchemaSpec extends AnyWordSpec {
  import SchemaSpec._

  "Schema" should {
    // check all places where `copy` method call is observed
    "copy extra" when {
      "def.withValidation" in { checkExtraGotCopied(_def) { _.withValidation(`maxLength` := 200) } }
      "def.toDefinition" in { checkExtraGotCopied(_def) { _.toDefinition("new-ref") } }
      "object.withField" in { checkExtraGotCopied(_obj) { _.withField("bar", `integer`) } }
      "object.dropField" in { checkExtraGotCopied(_obj) { _ dropField { _.name == "foo" } } }
      "object.withFieldsUpdated" in {
        checkExtraGotCopied(_obj) {
          _.withFieldsUpdated {
            case f if f.name == "foo" => f.copy(tpe = `boolean`)
          }
        }
      }
    }
  }
}

object SchemaSpec {
  import `string`.Format._

  private val _obj = `object`[Any](
    Field("foo", `string`)
  )

  private val _def = `def`[String](
    "some-ref",
    `string`(`date-time`)
  )

  private def checkExtraGotCopied[T, S[_] <: Schema[_]](schema: S[T])(mod: S[T] => S[T])(implicit pos: Position): Unit = {
    val schemaWithExtra = schema
      .withTitle("some title")
      .withDescription("some description")
      .withDiscriminationKey("_key")

    val resultingSchema = mod(schemaWithExtra.asInstanceOf[S[T]])

    resultingSchema.title.value shouldBe "some title"
    resultingSchema.description.value shouldBe "some description"
    resultingSchema.discriminationKey.value shouldBe "_key"
  }
}
