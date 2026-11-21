package petshop

import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec
import upickle.default.ReadWriter

// ---- user code: Scala 3 enums with upickle
object UPickle3Codecs {
  given ReadWriter[Suit]  = ReadWriter.derived
  given ReadWriter[Mark]  = ReadWriter.derived
  given ReadWriter[Card3] = ReadWriter.derived
}

/** Scala 3 only: enums, and `given`s next to `profiles.upickle.*`. */
class UPickle3Spec extends AnyWordSpec {
  import Acceptance.*
  import UPickle3Codecs.given

  private def rendered(s: Schema[?])(using profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsU.*
    byDraft(v => s.asU(v).render(), v => s.asU(v).render(), v => s.asU(v).render())
  }

  "profiles.upickle.*" should {
    "describe what upickle writes for Scala 3 enums" in {
      import com.github.andyglow.jsonschema.profiles.upickle.*
      val docs = Samples3.cards.map(upickle.default.write(_))
      docs shouldBe List("""{"suit":"Hearts","mark":"Dot"}""", """{"suit":"Spades","mark":{"$type":"Line","length":3}}""")
      assertValid(rendered(Schemas3.card), docs)
    }
    "let a local given profile win" in {
      import com.github.andyglow.jsonschema.profiles.upickle.*
      given mine: JsonSchemaProfile = JsonSchemaProfile.Legacy
      summon[JsonSchemaProfile] shouldBe theSameInstanceAs(mine)
      Schemas.pet.asU(Version.Draft12("x")).render() should include("\"age\":{\"type\":\"integer\"}") // Legacy, through the import's asU
    }
  }
}
