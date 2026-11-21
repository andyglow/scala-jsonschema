package petshop

import com.github.andyglow.jsonschema.PlayProfile
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import json.schema.JsonSchemaProfile.SumStyle
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{Json, JsonConfiguration, JsonNaming, OFormat}

// ---- user code: members nested in the trait's companion (play's Scala 2 macro can't derive this family; its Scala 3 one can)
object Play3ToyCodecs {
  given OFormat[Toy.Ball]      = Json.format[Toy.Ball]
  given OFormat[Toy.Kite.type] = Json.format[Toy.Kite.type]
  given OFormat[Toy]           = Json.format[Toy]
  given OFormat[Box]           = Json.format[Box]
}

/** Scala 3 only: `given`s next to `profiles.play.*`, and a family nested in its companion. */
class PlayProfile3Spec extends AnyWordSpec {
  import Acceptance.*

  private def rendered(s: Schema[?])(using profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsPlay.*
    byDraft(v => Json.stringify(s.asPlay(v)), v => Json.stringify(s.asPlay(v)), v => Json.stringify(s.asPlay(v)))
  }

  "profiles.play.*" should {
    "let a local given profile win" in {
      import com.github.andyglow.jsonschema.profiles.play.*
      given mine: JsonSchemaProfile = JsonSchemaProfile.Legacy
      summon[JsonSchemaProfile] shouldBe theSameInstanceAs(mine)
      Schemas.pet.asPlay(Version.Draft12("x")).toString should include("\"age\":{\"type\":\"integer\"}") // Legacy, through the import's asPlay
    }
    "let an imported project profile in the same scope win" in {
      import com.github.andyglow.jsonschema.profiles.play.*
      import PlayProjectSettings.*
      summon[JsonSchemaProfile] shouldBe theSameInstanceAs(PlayProjectSettings.profile)
      Schemas.pet.asPlay(Version.Draft12("x")).toString should include("\"age\":{\"type\":\"integer\"}")
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
    "describe members nested in the trait's companion (_type: petshop.Toy.Ball)" in {
      import com.github.andyglow.jsonschema.profiles.play.*
      import Play3ToyCodecs.given
      val docs = Samples.boxes.map(b => Json.stringify(Json.toJson(b)))
      docs.mkString should include("petshop.Toy.Ball")
      assertValid(rendered(Schemas.box), docs)
    }
  }
}
