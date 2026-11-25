package petshop

import _root_.spray.json._
import com.github.andyglow.jsonschema.SprayProfile
import com.github.andyglow.jsonschema.harness.CompileErrors
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import json.schema.JsonSchemaProfile.{MemberName, SingletonStyle, SumStyle}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

// ---- user code: spray-json derives products only; the formats of the families are hand-written
object SprayCodecs extends DefaultJsonProtocol {
  implicit val dogF: RootJsonFormat[Dog] = jsonFormat1(Dog.apply)
  implicit object AnimalF extends RootJsonFormat[Animal] {
    def write(a: Animal): JsValue = a match {
      case d: Dog => JsObject(d.toJson.asJsObject.fields + ("type" -> JsString("Dog")))
      case Fish   => JsObject("type" -> JsString("Fish"))
    }
    def read(j: JsValue): Animal = j.asJsObject.fields("type") match {
      case JsString("Dog") => j.convertTo[Dog]
      case _               => Fish
    }
  }
  implicit val petF: RootJsonFormat[Pet] = jsonFormat3(Pet.apply)

  // a format written to match @discriminator("kind") on Shape: the annotation describes it
  implicit val circleF: RootJsonFormat[Circle] = jsonFormat1(Circle.apply)
  implicit val squareF: RootJsonFormat[Square] = jsonFormat1(Square.apply)
  implicit object ShapeF extends RootJsonFormat[Shape] {
    def write(s: Shape): JsValue = s match {
      case c: Circle => JsObject(c.toJson.asJsObject.fields + ("kind" -> JsString("petshop.Circle")))
      case q: Square => JsObject(q.toJson.asJsObject.fields + ("kind" -> JsString("petshop.Square")))
    }
    def read(j: JsValue): Shape = j.asJsObject.fields("kind") match {
      case JsString("petshop.Circle") => j.convertTo[Circle]
      case _                          => j.convertTo[Square]
    }
  }
}

object SprayNullCodecs extends DefaultJsonProtocol with NullOptions {
  implicit val petF: RootJsonFormat[Pet] = jsonFormat3(Pet.apply)(implicitly, implicitly, SprayCodecs.AnimalF, implicitly)
}

class SprayAcceptanceSpec extends AnyWordSpec {
  import Acceptance._

  private def rendered(s: Schema[_])(implicit profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsSpray._
    byDraft(v => s.asSpray(v).compactPrint, v => s.asSpray(v).compactPrint, v => s.asSpray(v).compactPrint)
  }
  private def written[T: JsonWriter](xs: List[T]): List[String] = xs.map(_.toJson.compactPrint)

  // the shape AnimalF writes, stated
  private val stated = SprayProfile.default.withSum(SumStyle.Discriminator("type")).withMemberName(MemberName.SimpleName).withSingleton(SingletonStyle.AsTagged)

  "profiles.spray._" should {
    import com.github.andyglow.jsonschema.profiles.spray._
    import SprayCodecs._

    "not describe a hand-written family format by itself (a deliberate red: state it)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.spray.mkString("[", ",", "]")) shouldBe true
      assertInvalid(rendered(Schemas.pet), written(Samples.pets))
    }
    "render with the import's own asSpray" in {
      Schemas.pet.asSpray(Version.Draft12("x")).compactPrint shouldBe rendered(Schemas.pet).apply(Version.Draft12("x"))
    }
    "follow @discriminator, which describes a hand-written format" in assertValid(rendered(Schemas.shape), written(Samples.shapes))
    // spray's DefaultJsonProtocol writes only Map[String, V], so there is no output of the other key types to validate
    "reject a Map key the key type can't have (#466)" in assertBadKeysRejected(rendered(_))
  }

  "SprayProfile.default with the shape stated" should {
    import SprayCodecs._

    "describe AnimalF" in {
      assertValid(rendered(Schemas.pet)(stated), written(Samples.pets))
      crossCheckRow(rendered(Schemas.pet)(stated)) shouldBe List(
        "circe semiauto"   -> 0,
        "circe configured" -> 2, // the same shape: {"type": "Dog", ...}
        "play default"     -> 0,
        "play _kind"       -> 0,
        "upickle"          -> 0,
        "json4s"           -> 0,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 2
      )
    }
    "accept both Option encodings (DefaultJsonProtocol omits None, NullOptions writes null)" in {
      val docs = written(Samples.pets)(SprayNullCodecs.petF)
      docs.last should include("\"age\":null")
      assertValid(rendered(Schemas.pet)(stated), docs)
    }
  }

  "profiles.spray._ next to a profile of your own" should {
    "be an ambiguity that says what to do on Scala 2; yours wins on Scala 3 (reviewer11 A1)" in {
      val outcome = CompileErrors.outcome(assertCompiles("""
        import com.github.andyglow.jsonschema.profiles.spray._
        implicit val mine: json.schema.JsonSchemaProfile = json.schema.JsonSchemaProfile.Legacy
        implicitly[json.schema.JsonSchemaProfile]
      """))
      if (Acceptance.isScala3) outcome shouldBe None
      else outcome.get should include("profiles.spray._ already provides a JsonSchemaProfile (SprayProfile.default). To use your own profile, import com.github.andyglow.jsonschema.AsSpray._")
    }
  }
}
