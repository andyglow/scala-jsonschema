package petshop

import com.github.andyglow.jsonschema.UPickleProfile
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

// ---- user code: upickle.default
object UPickleDefaultCodecs {
  import _root_.upickle.default.{macroRW, ReadWriter => RW}
  implicit val dogRW: RW[Dog]          = macroRW
  implicit val fishRW: RW[Fish.type]   = macroRW
  implicit val animalRW: RW[Animal]    = macroRW
  implicit val petRW: RW[Pet]          = macroRW
  implicit val sedanRW: RW[Sedan]      = macroRW
  implicit val coupeRW: RW[Coupe.type] = macroRW
  implicit val carRW: RW[Car]          = macroRW
  implicit val bikeRW: RW[Bike]        = macroRW
  implicit val vehicleRW: RW[Vehicle]  = macroRW
  implicit val garageRW: RW[Garage]    = macroRW
  implicit val redRW: RW[Red.type]     = macroRW
  implicit val greenRW: RW[Green.type] = macroRW
  implicit val colorRW: RW[Color]      = macroRW
  implicit val paintRW: RW[Paint]      = macroRW
  implicit val readingRW: RW[Reading]  = macroRW
  implicit val circleRW: RW[Circle]    = macroRW
  implicit val squareRW: RW[Square]    = macroRW
  implicit val shapeRW: RW[Shape]      = macroRW
  implicit val cardRW: RW[Card]        = macroRW
  implicit val cashRW: RW[Cash]        = macroRW
  implicit val paymentRW: RW[Payment]  = macroRW
}

// ---- user code: upickle API objects of your own
object MyPickler extends _root_.upickle.AttributeTagged { override def tagName = "kind" }
object MyPicklerCodecs {
  import MyPickler.{macroRW, ReadWriter => RW}
  implicit val dogRW: RW[Dog]        = macroRW
  implicit val fishRW: RW[Fish.type] = macroRW
  implicit val animalRW: RW[Animal]  = macroRW
  implicit val petRW: RW[Pet]        = macroRW
}
object FullNames extends _root_.upickle.AttributeTagged { override def objectTypeKeyWriteFullyQualified = true }
object FullNamesCodecs {
  import FullNames.{macroRW, ReadWriter => RW}
  implicit val dogRW: RW[Dog]        = macroRW
  implicit val fishRW: RW[Fish.type] = macroRW
  implicit val animalRW: RW[Animal]  = macroRW
  implicit val petRW: RW[Pet]        = macroRW
}
object LowerTags extends _root_.upickle.AttributeTagged {
  override def objectTypeKeyWriteMap(s: CharSequence): CharSequence = s.toString.toLowerCase
  override def objectTypeKeyReadMap(s: CharSequence): CharSequence  = s.toString.capitalize
}
object LowerTagsCodecs {
  import LowerTags.{macroRW, ReadWriter => RW}
  implicit val dogRW: RW[Dog]        = macroRW
  implicit val fishRW: RW[Fish.type] = macroRW
  implicit val animalRW: RW[Animal]  = macroRW
  implicit val petRW: RW[Pet]        = macroRW
}
object NoNulls     extends _root_.upickle.AttributeTagged { override def optionsAsNulls = false                                                             }
object SnakeFields extends _root_.upickle.AttributeTagged { override def objectAttributeKeyWriteMap(s: CharSequence): CharSequence = s.toString.toUpperCase }

object UPickleSchemas {
  import com.github.andyglow.jsonschema.AsU.toValue // the Some(1) default's value
  val reading: Schema[Reading] = json.Json.schema[Reading]
}

class UPickleAcceptanceSpec extends AnyWordSpec {
  import Acceptance._

  // the schema as `asU` renders it under the profile in scope where this is called
  private def rendered(s: Schema[_])(implicit profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsU._
    byDraft(v => s.asU(v).render(), v => s.asU(v).render(), v => s.asU(v).render())
  }
  private def written[T](xs: List[T])(implicit w: _root_.upickle.default.Writer[T]): List[String] = xs.map(x => _root_.upickle.default.write(x))

  "profiles.upickle._ (upickle.default)" should {
    import com.github.andyglow.jsonschema.profiles.upickle._
    import UPickleDefaultCodecs._

    "describe what macroRW writes ($type, case objects as strings, null for None)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.upickle.mkString("[", ",", "]")) shouldBe true
      assertValid(rendered(Schemas.pet), written(Samples.pets))
    }
    "render with the import's own asU" in {
      Schemas.pet.asU(Version.Draft12("x")).render() shouldBe rendered(Schemas.pet).apply(Version.Draft12("x"))
    }
    "require an Option key, as upickle's reader does (a deliberate red)" in {
      val absent = """{"name":"Nemo","animal":"Fish"}"""
      Try(_root_.upickle.default.read[Pet](absent)).isFailure shouldBe true
      assertInvalid(rendered(Schemas.pet), List(absent))
    }
    "a nested family, flattened" in assertValid(rendered(Schemas.garage), written(List(Samples.garage)))
    "a family of case objects only" in assertValid(rendered(Schemas.paint), written(Samples.paints))
    "Option fields with defaults (= None, = Some(1)): not required, and dropped when equal to the default" in {
      written(Samples.readings).head shouldBe """{"id":"r1","note":null}"""
      assertValid(rendered(UPickleSchemas.reading), written(Samples.readings))
      assertInvalid(rendered(UPickleSchemas.reading), List("""{"id":"r3"}""")) // note has no default: its key is required
    }
    "ignore @discriminator, which upickle's derivation never sees (R4B)" in assertValid(rendered(Schemas.shape), written(Samples.shapes))
    "tag a member that has an implicit Schema of its own" in assertValid(rendered(Schemas.payment), written(Samples.payments))
    "be the cross-check row of upickle" in {
      crossCheckRow(rendered(Schemas.pet)) shouldBe List(
        "circe semiauto"   -> 0,
        "circe configured" -> 0,
        "play default"     -> 0,
        "play _kind"       -> 0,
        "upickle"          -> 2,
        "json4s"           -> 0,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 0
      )
    }
  }

  "UPickleProfile.fromApi" should {
    "read tagName" in {
      implicit val profile: JsonSchemaProfile = UPickleProfile.fromApi(MyPickler)
      assertValid(rendered(Schemas.pet), Samples.pets.map(p => MyPickler.write(p)(MyPicklerCodecs.petRW)))
    }
    "read objectTypeKeyWriteFullyQualified" in {
      implicit val profile: JsonSchemaProfile = UPickleProfile.fromApi(FullNames)
      val docs                                = Samples.pets.map(p => FullNames.write(p)(FullNamesCodecs.petRW))
      docs.mkString should include("petshop.Dog")
      assertValid(rendered(Schemas.pet), docs)
    }
    "apply objectTypeKeyWriteMap to tags, not to case object strings (as upickle does)" in {
      implicit val profile: JsonSchemaProfile = UPickleProfile.fromApi(LowerTags)
      val docs                                = Samples.pets.map(p => LowerTags.write(p)(LowerTagsCodecs.petRW))
      docs.mkString should (include("\"dog\"") and include("\"Fish\""))
      assertValid(rendered(Schemas.pet), docs)
    }
    "fail fast on settings no profile describes (R6A)" in {
      (the[IllegalArgumentException] thrownBy UPickleProfile.fromApi(NoNulls)).getMessage should include("optionsAsNulls = false")
      (the[IllegalArgumentException] thrownBy UPickleProfile.fromApi(SnakeFields)).getMessage should include("objectAttributeKeyWriteMap renames fields (name -> NAME)")
    }
  }
}
