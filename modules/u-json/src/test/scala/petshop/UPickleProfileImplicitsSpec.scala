package petshop

import com.github.andyglow.jsonschema.harness.CompileErrors
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

/** reviewer11 A1: `profiles.upickle._` next to a profile of your own. Scala 2 reports an ambiguity, with a message that says what to do; on Scala 3 the local one wins (UPickle3Spec). */
class UPickleProfileImplicitsSpec extends AnyWordSpec {

  "profiles.upickle._ and a local implicit profile" should {
    if (Acceptance.isScala3) "compile (Scala 3)" in {
      CompileErrors.outcome(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.upickle._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """)) shouldBe None
    }
    else
      "be an ambiguity that says what to do (Scala 2)" in {
        CompileErrors.errorOf(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.upickle._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """)) should include("profiles.upickle._ already provides a JsonSchemaProfile (the one of upickle.default). To use your own profile, import com.github.andyglow.jsonschema.AsU._")
      }
  }
}
