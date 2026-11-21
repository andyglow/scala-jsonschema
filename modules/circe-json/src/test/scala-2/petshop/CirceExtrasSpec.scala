package petshop

import com.github.andyglow.jsonschema.CirceProfile
import io.circe.Encoder
import io.circe.syntax._
import json.schema.JsonSchemaProfile
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

// ---- user code: circe-generic-extras (Scala 2)
object CirceExtrasCodecs {
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.semiauto._
  implicit val config: Configuration     = Configuration.default.withDiscriminator("type")
  implicit val dogE: Encoder[Dog]        = deriveConfiguredEncoder
  implicit val fishE: Encoder[Fish.type] = deriveConfiguredEncoder
  implicit val animalE: Encoder[Animal]  = deriveConfiguredEncoder
  implicit val petE: Encoder[Pet]        = deriveConfiguredEncoder
}
// renamed constructors (reviewer12 S2), with a discriminator and with wrapper objects
object CirceExtrasSnakeTagCodecs {
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.semiauto._
  implicit val config: Configuration     = Configuration.default.withDiscriminator("t").withSnakeCaseConstructorNames
  implicit val dogE: Encoder[Dog]        = deriveConfiguredEncoder
  implicit val fishE: Encoder[Fish.type] = deriveConfiguredEncoder
  implicit val animalE: Encoder[Animal]  = deriveConfiguredEncoder
  implicit val petE: Encoder[Pet]        = deriveConfiguredEncoder
}
object CirceExtrasKebabCodecs {
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.semiauto._
  implicit val config: Configuration     = Configuration.default.withKebabCaseConstructorNames
  implicit val dogE: Encoder[Dog]        = deriveConfiguredEncoder
  implicit val fishE: Encoder[Fish.type] = deriveConfiguredEncoder
  implicit val animalE: Encoder[Animal]  = deriveConfiguredEncoder
  implicit val petE: Encoder[Pet]        = deriveConfiguredEncoder
}
object CirceExtrasSnakeCodecs {
  import io.circe.generic.extras.Configuration
  implicit val config: Configuration = Configuration.default.withDiscriminator("type").withSnakeCaseMemberNames
}

// ---- user code: value classes in a family, circe-generic semiauto (its Scala 3 derivation has no Mirror for a value class)
object CirceValueClassCodecs {
  import io.circe.generic.semiauto._
  implicit val numE: Encoder[Num]     = deriveEncoder
  implicit val labelE: Encoder[Label] = deriveEncoder
  implicit val identE: Encoder[Ident] = deriveEncoder
  implicit val badgeE: Encoder[Badge] = deriveEncoder
}

/** Scala 2 only: circe-generic-extras' configured codecs, its Configuration's fields passed to `CirceProfile.configured`; value class members. */
class CirceExtrasSpec extends AnyWordSpec {
  import Acceptance._

  "profiles.circe._" should {
    "describe value class members as semiauto writes them, objects in the wrapper" in {
      import com.github.andyglow.jsonschema.profiles.circe._
      import CirceValueClassCodecs._
      assertValid(
        byDraft(v => Schemas.badge.asCirce(v).noSpaces, v => Schemas.badge.asCirce(v).noSpaces, v => Schemas.badge.asCirce(v).noSpaces),
        Samples.badges.map(_.asJson.noSpaces)
      )
    }
  }

  "CirceProfile.configured(cfg.discriminator, cfg.transformConstructorNames, cfg.transformMemberNames)" should {
    "describe what deriveConfiguredEncoder writes" in {
      import com.github.andyglow.jsonschema.AsCirce._
      import CirceExtrasCodecs._
      implicit val profile: JsonSchemaProfile = CirceProfile.configured(config.discriminator, config.transformConstructorNames, config.transformMemberNames)
      val docs                                = Samples.pets.map(_.asJson.noSpaces)
      sameJson(docs.mkString("[", ",", "]"), LibraryOutputs.circeConfigured.mkString("[", ",", "]")) shouldBe true
      assertValid(byDraft(v => Schemas.pet.asCirce(v).noSpaces, v => Schemas.pet.asCirce(v).noSpaces, v => Schemas.pet.asCirce(v).noSpaces), docs)
    }
    "follow renamed constructors, tagged (snake_case + discriminator)" in {
      import com.github.andyglow.jsonschema.AsCirce._
      import CirceExtrasSnakeTagCodecs._
      implicit val profile: JsonSchemaProfile = CirceProfile.configured(config.discriminator, config.transformConstructorNames, config.transformMemberNames)
      val docs                                = Samples.pets.map(_.asJson.noSpaces)
      docs.mkString should (include("\"t\":\"dog\"") and include("\"t\":\"fish\""))
      assertValid(byDraft(v => Schemas.pet.asCirce(v).noSpaces, v => Schemas.pet.asCirce(v).noSpaces, v => Schemas.pet.asCirce(v).noSpaces), docs)
    }
    "follow renamed constructors, wrapped (kebab-case, no discriminator)" in {
      import com.github.andyglow.jsonschema.AsCirce._
      import CirceExtrasKebabCodecs._
      implicit val profile: JsonSchemaProfile = CirceProfile.configured(config.discriminator, config.transformConstructorNames, config.transformMemberNames)
      val docs                                = Samples.pets.map(_.asJson.noSpaces)
      docs.mkString should (include("{\"dog\":") and include("{\"fish\":"))
      assertValid(byDraft(v => Schemas.pet.asCirce(v).noSpaces, v => Schemas.pet.asCirce(v).noSpaces, v => Schemas.pet.asCirce(v).noSpaces), docs)
    }
    "fail fast on snake_case member names (R6A, reviewer11 A4)" in {
      import CirceExtrasSnakeCodecs.config
      val e = the[IllegalArgumentException] thrownBy CirceProfile.configured(config.discriminator, config.transformConstructorNames, config.transformMemberNames)
      e.getMessage should include("transformMemberNames renames fields (fooBar -> foo_bar)")
    }
  }

  "profiles.circe._ next to a profile of your own" should {
    "be an ambiguity that says what to do (reviewer11 A1)" in {
      com.github.andyglow.jsonschema.harness.CompileErrors.errorOf(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.circe._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """)) should include("profiles.circe._ already provides a JsonSchemaProfile (circe-generic's). To use your own profile, import com.github.andyglow.jsonschema.AsCirce._")
    }
  }
}
