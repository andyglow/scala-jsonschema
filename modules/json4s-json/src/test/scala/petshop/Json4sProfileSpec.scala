package petshop

import com.github.andyglow.jsonschema.Json4sProfile
import com.github.andyglow.jsonschema.harness.CompileErrors
import json.Schema.`oneof`
import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile.SumStyle
import org.json4s.{DefaultFormats, Formats, FullTypeHints, ShortTypeHints}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

/** Json4sProfile on the derived trees, without encoding (json4s 4 can't serialize case classes on Scala 3; the real-output suite is Json4sAcceptanceSpec, Scala 2). */
class Json4sProfileSpec extends AnyWordSpec {

  private def field(p: JsonSchemaProfile): Option[String] = p.lower(Schemas.pet).asInstanceOf[json.Schema.`object`[_]].fields.find(_.name == "animal").get.tpe match {
    case `oneof`(_, f) => f
    case other         => fail(s"not a oneof: $other")
  }

  "Json4sProfile.fromFormats" should {
    "leave members untagged without type hints" in {
      Json4sProfile.default.sumStyle shouldBe SumStyle.Untagged
      field(Json4sProfile.default) shouldBe None
    }
    "tag a family whose classes all have hints, with the hints' field" in {
      field(Json4sProfile.fromFormats(DefaultFormats + ShortTypeHints(List(classOf[Dog], Fish.getClass), "kind"))) shouldBe Some("kind")
      field(Json4sProfile.fromFormats(DefaultFormats + FullTypeHints(List(classOf[Dog], Fish.getClass)))) shouldBe Some("jsonClass")
    }
    "leave a family without hints untagged next to one with them (hints are per class)" in {
      val p = Json4sProfile.fromFormats(DefaultFormats + ShortTypeHints(List(classOf[Dog], Fish.getClass), "kind"))
      p.lower(Schemas.shape) match {
        case `oneof`(_, f) => f shouldBe None
        case other         => fail(s"not a oneof: $other")
      }
    }
    "fail when the hints cover only some members of a family (R6A)" in {
      val p = Json4sProfile.fromFormats(DefaultFormats + ShortTypeHints(List(classOf[Dog])))
      (the[IllegalArgumentException] thrownBy p.lower(Schemas.pet)).getMessage should include(
        "Formats.typeHints cover only some members of a sealed family (with a hint: petshop.Dog; without: petshop.Fish)"
      )
    }
    "fail when a family's hints use different fields (R6A)" in {
      val p = Json4sProfile.fromFormats(DefaultFormats + ShortTypeHints(List(classOf[Dog]), "a") + ShortTypeHints(List(Fish.getClass), "b"))
      (the[IllegalArgumentException] thrownBy p.lower(Schemas.pet)).getMessage should include("use different fields (petshop.Dog: a, petshop.Fish: b)")
    }
  }

  "profiles.json4s._" should {
    "use the implicit Formats in scope" in {
      import com.github.andyglow.jsonschema.profiles.json4s._
      implicit val formats: Formats = DefaultFormats + ShortTypeHints(List(classOf[Dog], Fish.getClass), "kind")
      field(implicitly[JsonSchemaProfile]) shouldBe Some("kind")
    }
    "use DefaultFormats without one" in {
      import com.github.andyglow.jsonschema.profiles.json4s._
      field(implicitly[JsonSchemaProfile]) shouldBe None
    }
    "next to a profile of your own: Scala 2 reports an ambiguity that says what to do, on Scala 3 yours wins (reviewer11 A1)" in {
      val outcome = CompileErrors.outcome(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.json4s._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """))
      if (Acceptance.isScala3) outcome shouldBe None
      else outcome.get should include("profiles.json4s._ already provides a JsonSchemaProfile (Json4sProfile.default, no implicit Formats in scope). To use your own profile")
    }
  }
}
