package petshop

import com.github.andyglow.json.JsonFormatter
import com.github.andyglow.jsonschema.AsValue
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

/** The cross-check rows of the profiles with no library behind them: Neutral (the default) and Legacy (0.7) describe no library's derived sum codec. The bridge rows are in each bridge's spec. */
class NeutralLegacyAcceptanceSpec extends AnyWordSpec {

  private def render(s: Schema[_], p: JsonSchemaProfile): Version => String = Acceptance.rendered(s)(p)

  "Neutral" should {
    "be the profile when none is in scope" in {
      JsonFormatter.format(AsValue.schema(Schemas.pet, Version.Draft12("x"))) shouldBe JsonFormatter.format(AsValue.schema(Schemas.pet, Version.Draft12("x"))(implicitly, JsonSchemaProfile.Neutral))
    }
    "accept only the libraries' output whose sum shape happens to match (upickle's case object string, json4s' untagged Dog)" in {
      Acceptance.crossCheckRow(render(Schemas.pet, JsonSchemaProfile.Neutral)) shouldBe List(
        "circe semiauto"   -> 0,
        "circe configured" -> 0,
        "play default"     -> 0,
        "play _kind"       -> 0,
        "upickle"          -> 1,
        "json4s"           -> 1,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 0
      )
    }
    "follow @discriminator on a hand-written codec's family (it honors the annotations)" in {
      Acceptance.assertValid(render(Schemas.shape, JsonSchemaProfile.Neutral), List("""{"kind":"petshop.Circle","r":1.5}""", """{"kind":"petshop.Square","a":2}"""))
    }
  }

  "Legacy" should {
    "accept only json4s' untagged Dog (an Option field is its element's schema, so `null` is rejected)" in {
      Acceptance.crossCheckRow(render(Schemas.pet, JsonSchemaProfile.Legacy)) shouldBe List(
        "circe semiauto"   -> 0,
        "circe configured" -> 0,
        "play default"     -> 0,
        "play _kind"       -> 0,
        "upickle"          -> 0,
        "json4s"           -> 1,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 0
      )
    }
  }
}
