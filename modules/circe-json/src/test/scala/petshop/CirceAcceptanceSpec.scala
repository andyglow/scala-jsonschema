package petshop

import com.github.andyglow.jsonschema.CirceProfile
import io.circe.Encoder
import io.circe.syntax._
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

// ---- user code: circe-generic semiauto
object CirceSemiautoCodecs {
  import io.circe.generic.semiauto._
  implicit val dogE: Encoder[Dog]            = deriveEncoder
  implicit val fishE: Encoder[Fish.type]     = deriveEncoder
  implicit val animalE: Encoder[Animal]      = deriveEncoder
  implicit val petE: Encoder[Pet]            = deriveEncoder
  implicit val sedanE: Encoder[Sedan]        = deriveEncoder
  implicit val coupeE: Encoder[Coupe.type]   = deriveEncoder
  implicit val bikeE: Encoder[Bike]          = deriveEncoder
  implicit val vehicleE: Encoder[Vehicle]    = deriveEncoder
  implicit val garageE: Encoder[Garage]      = deriveEncoder
  implicit val redE: Encoder[Red.type]       = deriveEncoder
  implicit val greenE: Encoder[Green.type]   = deriveEncoder
  implicit val colorE: Encoder[Color]        = deriveEncoder
  implicit val paintE: Encoder[Paint]        = deriveEncoder
  implicit val readingE: Encoder[Reading]    = deriveEncoder
  implicit val circleE: Encoder[Circle]      = deriveEncoder
  implicit val squareE: Encoder[Square]      = deriveEncoder
  implicit val shapeE: Encoder[Shape]        = deriveEncoder
  implicit val cardE: Encoder[Card]          = deriveEncoder
  implicit val cashE: Encoder[Cash]          = deriveEncoder
  implicit val paymentE: Encoder[Payment]    = deriveEncoder
  implicit val ballE: Encoder[Toy.Ball]      = deriveEncoder
  implicit val kiteE: Encoder[Toy.Kite.type] = deriveEncoder
  implicit val toyE: Encoder[Toy]            = deriveEncoder
  implicit val boxE: Encoder[Box]            = deriveEncoder
  // Map keys (#466): circe has KeyEncoders for Int, Long and UUID (toString), none for Boolean or Char
  implicit val intStockE: Encoder[IntStock]   = deriveEncoder
  implicit val uuidStockE: Encoder[UuidStock] = deriveEncoder
}

object CirceSchemas {
  import com.github.andyglow.jsonschema.AsCirce.toValue // the Some(1) default's value
  val reading: Schema[Reading] = json.Json.schema[Reading]

  // circe writes a Char / java.lang.Character as a one-character string (#463)
  case class Initials(first: Char, last: Character)
  implicit val initialsE: Encoder[Initials] = io.circe.generic.semiauto.deriveEncoder
  val initials: Schema[Initials]            = json.Json.schema[Initials]
  val someInitials: List[Initials]          = List(Initials('a', 'Z'), Initials(' ', '\n'), Initials('é', '.'))
}

class CirceAcceptanceSpec extends AnyWordSpec {
  import Acceptance._

  // the schema as `asCirce` renders it under the profile in scope where this is called
  private def rendered(s: Schema[_])(implicit profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsCirce._
    byDraft(v => s.asCirce(v).noSpaces, v => s.asCirce(v).noSpaces, v => s.asCirce(v).noSpaces)
  }
  private def written[T: Encoder](xs: List[T]): List[String] = xs.map(_.asJson.noSpaces)

  "profiles.circe._ (circe-generic semiauto)" should {
    import com.github.andyglow.jsonschema.profiles.circe._
    import CirceSemiautoCodecs._

    "describe what deriveEncoder writes (wrapper objects, null for None)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.circeSemiauto.mkString("[", ",", "]")) shouldBe true
      assertValid(rendered(Schemas.pet), written(Samples.pets))
    }
    "render with the import's own asCirce" in {
      Schemas.pet.asCirce(Version.Draft12("x")).noSpaces shouldBe rendered(Schemas.pet).apply(Version.Draft12("x"))
    }
    "a nested family, flattened" in assertValid(rendered(Schemas.garage), written(List(Samples.garage)))
    "members nested in the trait's companion" in assertValid(rendered(Schemas.box), written(Samples.boxes))
    "a family of case objects only" in assertValid(rendered(Schemas.paint), written(Samples.paints))
    "Option fields with defaults" in assertValid(rendered(CirceSchemas.reading), written(Samples.readings))
    "Char and java.lang.Character fields (#463)" in assertValid(rendered(CirceSchemas.initials), written(CirceSchemas.someInitials))
    "ignore @discriminator, which circe's derivation never sees (R4B)" in assertValid(rendered(Schemas.shape), written(Samples.shapes))
    "wrap a member that has an implicit Schema of its own" in assertValid(rendered(Schemas.payment), written(Samples.payments))
    "Map keys: negative Int and Long keys, UUID keys (#466)" in {
      written(List(Samples.intStock)) shouldBe List("""{"byId":{"-1":1,"0":2,"42":3},"byLong":{"-4294967296":1}}""")
      assertValid(rendered(Schemas.intStock), written(List(Samples.intStock)))
      assertValid(rendered(Schemas.uuidStock), written(List(Samples.uuidStock)))
      assertBadKeysRejected(rendered(_))
    }
    "be the cross-check row of circe semiauto" in {
      crossCheckRow(rendered(Schemas.pet)) shouldBe List(
        "circe semiauto"   -> 2,
        "circe configured" -> 0,
        "play default"     -> 0,
        "play _kind"       -> 0,
        "upickle"          -> 0,
        "json4s"           -> 0,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 0
      )
    }
  }

  "CirceProfile.configured" should {
    "with a discriminator, tag the members (the configured codecs' shape)" in {
      implicit val profile: JsonSchemaProfile = CirceProfile.configured(Some("type"))
      assertValid(rendered(Schemas.pet), LibraryOutputs.circeConfigured)
      crossCheckRow(rendered(Schemas.pet)).toMap.apply("circe semiauto") shouldBe 0
    }
    "rename the members with transformConstructorNames" in {
      implicit val profile: JsonSchemaProfile = CirceProfile.configured(None, _.toLowerCase)
      assertValid(rendered(Schemas.pet), List("""{"name":"Rex","age":3,"animal":{"dog":{"name":"Rex"}}}""", """{"name":"Nemo","age":null,"animal":{"fish":{}}}"""))
    }
    "fail fast on transformMemberNames that rename fields (R6A, reviewer11 A4)" in {
      val e = the[IllegalArgumentException] thrownBy CirceProfile.configured(Some("type"), identity, _.toUpperCase)
      e.getMessage should include("transformMemberNames renames fields (name -> NAME)")
    }
  }
}
