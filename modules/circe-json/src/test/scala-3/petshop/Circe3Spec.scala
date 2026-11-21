package petshop

import com.github.andyglow.jsonschema.CirceProfile
import io.circe.Encoder
import io.circe.derivation.{Configuration, ConfiguredEncoder}
import io.circe.syntax.*
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import json.schema.JsonSchemaProfile.SumStyle
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

// ---- user code: circe-core's configured derivation (Scala 3)
object Circe3ConfiguredCodecs {
  given Configuration   = Configuration.default.withDiscriminator("type")
  given Encoder[Dog]    = ConfiguredEncoder.derived
  given Encoder[Animal] = ConfiguredEncoder.derived
  given Encoder[Pet]    = ConfiguredEncoder.derived
}

// ---- user code: renamed constructors (reviewer12 S2), with a discriminator and with wrapper objects
object Circe3SnakeCodecs {
  given Configuration   = Configuration.default.withDiscriminator("t").withSnakeCaseConstructorNames
  given Encoder[Dog]    = ConfiguredEncoder.derived
  given Encoder[Animal] = ConfiguredEncoder.derived
  given Encoder[Pet]    = ConfiguredEncoder.derived
}
object Circe3KebabCodecs {
  given Configuration   = Configuration.default.withKebabCaseConstructorNames
  given Encoder[Dog]    = ConfiguredEncoder.derived
  given Encoder[Animal] = ConfiguredEncoder.derived
  given Encoder[Pet]    = ConfiguredEncoder.derived
}
object Circe3ToyCodecs {
  given Configuration     = Configuration.default.withDiscriminator("t")
  given Encoder[Toy.Ball] = ConfiguredEncoder.derived
  given Encoder[Toy]      = ConfiguredEncoder.derived
  given Encoder[Box]      = ConfiguredEncoder.derived
}

// ---- user code: Scala 3 enums with circe-generic semiauto
object Circe3EnumCodecs {
  import io.circe.generic.semiauto.deriveEncoder
  given Encoder[Suit]  = deriveEncoder
  given Encoder[Mark]  = deriveEncoder
  given Encoder[Card3] = deriveEncoder
}

/** Scala 3 only: a `given Configuration` picked up by `profiles.circe.*`, enums, and `given`s next to the import. */
class Circe3Spec extends AnyWordSpec {
  import Acceptance.*

  private def rendered(s: Schema[?])(using profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsCirce.*
    byDraft(v => s.asCirce(v).noSpaces, v => s.asCirce(v).noSpaces, v => s.asCirce(v).noSpaces)
  }

  "profiles.circe.*" should {
    "pick up the given Configuration ConfiguredEncoder derives with" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      import Circe3ConfiguredCodecs.given
      val docs = Samples.pets.map(_.asJson.noSpaces)
      sameJson(docs.mkString("[", ",", "]"), LibraryOutputs.circeConfigured.mkString("[", ",", "]")) shouldBe true
      summon[JsonSchemaProfile].sumStyle shouldBe SumStyle.Discriminator("type")
      assertValid(rendered(Schemas.pet), docs)
      Schemas.pet.asCirce(Version.Draft12("x")).noSpaces shouldBe rendered(Schemas.pet).apply(Version.Draft12("x"))
    }
    "follow renamed constructors, tagged (snake_case + discriminator)" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      import Circe3SnakeCodecs.given
      val docs = Samples.pets.map(_.asJson.noSpaces)
      docs.mkString should (include("\"t\":\"dog\"") and include("\"t\":\"fish\""))
      assertValid(rendered(Schemas.pet), docs)
    }
    "follow renamed constructors, wrapped (kebab-case, no discriminator)" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      import Circe3KebabCodecs.given
      val docs = Samples.pets.map(_.asJson.noSpaces)
      docs.mkString should (include("{\"dog\":") and include("{\"fish\":"))
      assertValid(rendered(Schemas.pet), docs)
    }
    "describe members nested in the trait's companion" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      import Circe3ToyCodecs.given
      assertValid(rendered(Schemas.box), Samples.boxes.map(_.asJson.noSpaces))
    }
    "be circe-generic's without one" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      summon[JsonSchemaProfile].sumStyle shouldBe SumStyle.WrapperObject
    }
    "describe what semiauto writes for Scala 3 enums" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      import Circe3EnumCodecs.given
      val docs = Samples3.cards.map(_.asJson.noSpaces)
      assertValid(rendered(Schemas3.card), docs)
    }
    "let a local given profile win" in {
      import com.github.andyglow.jsonschema.profiles.circe.*
      given mine: JsonSchemaProfile = JsonSchemaProfile.Legacy
      summon[JsonSchemaProfile] shouldBe theSameInstanceAs(mine)
      Schemas.pet.asCirce(Version.Draft12("x")).noSpaces should include("\"age\":{\"type\":\"integer\"}") // Legacy, through the import's asCirce
    }
  }

  "CirceProfile.fromConfig" should {
    "read the discriminator and constructor names" in {
      val p = CirceProfile.fromConfig(Configuration.default.withDiscriminator("kind").withSnakeCaseConstructorNames)
      p.sumStyle shouldBe SumStyle.Discriminator("kind")
      p.memberName(Schemas.animalMember("Dog")) shouldBe "dog"
    }
    "fail fast on member names that rename fields (R6A)" in {
      val e = the[IllegalArgumentException] thrownBy CirceProfile.fromConfig(Configuration.default.withSnakeCaseMemberNames)
      e.getMessage should include("transformMemberNames renames fields (fooBar -> foo_bar)")
    }
  }
}
