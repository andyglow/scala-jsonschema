package petshop

import com.github.andyglow.jsonschema.harness.CompileErrors
import json.schema.JsonSchemaProfile
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

object PlayProjectSettings {
  implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy
}

/** reviewer11 A1: `profiles.play._` next to a profile of your own. Scala 2 reports an ambiguity, with a message that says what to do; on Scala 3 the closer one wins. */
class PlayProfileImplicitsSpec extends AnyWordSpec {

  private val advice = "profiles.play._ already provides a JsonSchemaProfile (from the implicit JsonConfiguration in scope). To use your own profile, import com.github.andyglow.jsonschema.AsPlay._"

  "profiles.play._ and a local implicit profile" should {
    if (Acceptance.isScala3) "compile, the local one winning (Scala 3: PlayProfile3Spec checks which)" in {
      CompileErrors.outcome(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.play._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """)) shouldBe None
    }
    else
      "be an ambiguity that says what to do (Scala 2)" in {
        CompileErrors.errorOf(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.play._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """)) should include(advice)
      }
  }

  "profiles.play._ and an imported profile, in the same scope" should {
    if (Acceptance.isScala3) "pick the project's (Scala 3)" in {
      CompileErrors.outcome(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.play._
        import petshop.PlayProjectSettings._
        implicitly[json.schema.JsonSchemaProfile]
      """)) shouldBe None
    }
    else
      "be an ambiguity that says what to do (Scala 2)" in {
        CompileErrors.errorOf(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.play._
        import petshop.PlayProjectSettings._
        implicitly[json.schema.JsonSchemaProfile]
      """)) should include(advice)
      }
  }

  "AsPlay._ and a profile of your own" should {
    "compile, the profile being yours" in {
      import com.github.andyglow.jsonschema.AsPlay._
      import PlayProjectSettings._
      implicitly[JsonSchemaProfile] shouldBe theSameInstanceAs(PlayProjectSettings.profile)
      Schemas.pet.asPlay(json.schema.Version.Draft12("x")).toString should include("\"age\":{\"type\":\"integer\"}") // Legacy
    }
  }
}
