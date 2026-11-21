package petshop

import com.github.andyglow.jsonschema.PlayProfile
import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile.SumStyle
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsonConfiguration, JsonNaming}

/** Scala 3 only: `given`s next to `profiles.play.*`. */
class PlayProfile3Spec extends AnyWordSpec {

  "profiles.play.*" should {
    "let a local given profile win" in {
      import com.github.andyglow.jsonschema.profiles.play.*
      given mine: JsonSchemaProfile = JsonSchemaProfile.Legacy
      summon[JsonSchemaProfile] shouldBe theSameInstanceAs(mine)
      Schemas.pet.asPlay(json.schema.Version.Draft12("x")).toString should include("\"age\":{\"type\":\"integer\"}") // Legacy, through the import's asPlay
    }
    "pick up a given JsonConfiguration" in {
      import com.github.andyglow.jsonschema.profiles.play.*
      given JsonConfiguration = JsonConfiguration(discriminator = "_kind", typeNaming = JsonNaming(_.split('.').last))
      summon[JsonSchemaProfile].sumStyle shouldBe SumStyle.Discriminator("_kind")
    }
    "be play's default without one" in {
      import com.github.andyglow.jsonschema.profiles.play.*
      summon[JsonSchemaProfile].sumStyle shouldBe PlayProfile.default.sumStyle
    }
  }
}
