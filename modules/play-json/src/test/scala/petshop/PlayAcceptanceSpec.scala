package petshop

import _root_.play.api.libs.json._
import com.github.andyglow.jsonschema.PlayProfile
import com.github.andyglow.jsonschema.harness.TriageB
import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.nowarn

// ---- user code: play-json with its default configuration
object PlayDefaultCodecs {
  implicit val fishF: OFormat[Fish.type]   = Json.format[Fish.type]
  implicit val dogF: OFormat[Dog]          = Json.format[Dog]
  implicit val animalF: OFormat[Animal]    = Json.format[Animal]
  implicit val petF: OFormat[Pet]          = Json.format[Pet]
  implicit val sedanF: OFormat[Sedan]      = Json.format[Sedan]
  implicit val coupeF: OFormat[Coupe.type] = Json.format[Coupe.type]
  implicit val bikeF: OFormat[Bike]        = Json.format[Bike]
  implicit val carF: OFormat[Car]          = Json.format[Car]
  @nowarn("msg=unreachable code") // play's Scala 2 macro, for a sub-family's members
  implicit val vehicleF: OFormat[Vehicle]  = Json.format[Vehicle]
  implicit val garageF: OFormat[Garage]    = Json.format[Garage]
  implicit val redF: OFormat[Red.type]     = Json.format[Red.type]
  implicit val greenF: OFormat[Green.type] = Json.format[Green.type]
  implicit val colorF: OFormat[Color]      = Json.format[Color]
  implicit val paintF: OFormat[Paint]      = Json.format[Paint]
  implicit val readingF: OFormat[Reading]  = Json.format[Reading]
  implicit val circleF: OFormat[Circle]    = Json.format[Circle]
  implicit val squareF: OFormat[Square]    = Json.format[Square]
  implicit val shapeF: OFormat[Shape]      = Json.format[Shape]
  implicit val cardF: OFormat[Card]        = Json.format[Card]
  implicit val cashF: OFormat[Cash]        = Json.format[Cash]
  implicit val paymentF: OFormat[Payment]  = Json.format[Payment]
  // play's Scala 3 macro has no format for a value class: written by hand, as objects
  implicit val numF: OFormat[Num]     = OFormat((__ \ "n").read[Int].map(Num.apply), (x: Num) => Json.obj("n" -> x.n))
  implicit val labelF: OFormat[Label] = OFormat((__ \ "text").read[String].map(Label.apply), (x: Label) => Json.obj("text" -> x.text))
  implicit val identF: OFormat[Ident] = Json.format[Ident]
  implicit val badgeF: OFormat[Badge] = Json.format[Badge]
  // Map keys (#466): play's KeyWrites cover AnyVal keys (toString); a UUID-keyed Map is written as an array of pairs
  implicit val intStockW: OWrites[IntStock]   = Json.writes[IntStock]
  implicit val flagStockW: OWrites[FlagStock] = Json.writes[FlagStock]
  implicit val charStockW: OWrites[CharStock] = Json.writes[CharStock]
  implicit val uuidStockW: OWrites[UuidStock] = Json.writes[UuidStock]
}

// ---- user code: play-json with a JsonConfiguration of its own (an implicit in scope, as play's macros need it)
object PlayCustomCodecs {
  implicit val cfg: JsonConfiguration = JsonConfiguration(
    discriminator = "_kind",
    typeNaming = JsonNaming(fullName => fullName.split('.').last), // "petshop.Dog" -> "Dog"
    optionHandlers = OptionHandlers.WritesNull
  )
  implicit val fishF: OFormat[Fish.type] = Json.format[Fish.type]
  implicit val dogF: OFormat[Dog]        = Json.format[Dog]
  implicit val animalF: OFormat[Animal]  = Json.format[Animal]
  implicit val petF: OFormat[Pet]        = Json.format[Pet]
}

object PlaySchemas {
  import com.github.andyglow.jsonschema.AsPlay.toValue // the Some(1) default's value
  val reading: Schema[Reading] = json.Json.schema[Reading]
}

class PlayAcceptanceSpec extends AnyWordSpec {
  import Acceptance._

  // the schema as `asPlay` renders it under the profile in scope where this is called
  private def rendered(s: Schema[_])(implicit profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsPlay._
    byDraft(v => Json.stringify(s.asPlay(v)), v => Json.stringify(s.asPlay(v)), v => Json.stringify(s.asPlay(v)))
  }
  private def written[T](xs: List[T])(implicit w: Writes[T]): List[String] = xs.map(x => Json.stringify(Json.toJson(x)))

  "profiles.play._ with play's default JsonConfiguration" should {
    import com.github.andyglow.jsonschema.profiles.play._
    import PlayDefaultCodecs._

    "describe what Json.format writes (Option field, case class and case object members)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.playDefault.mkString("[", ",", "]")) shouldBe true
      assertValid(rendered(Schemas.pet), written(Samples.pets))
    }
    "render with the import's own asPlay" in {
      Json.stringify(Schemas.pet.asPlay(Version.Draft12("x"))) shouldBe rendered(Schemas.pet).apply(Version.Draft12("x"))
    }
    "a nested family, flattened" in assertValid(rendered(Schemas.garage), written(List(Samples.garage)))
    "a schema derived right here, where the import's ToValue sees play's Writes[Animal] (was a compile error: no @typeHint)" in {
      val here = json.Json.schema[Pet]
      assertValid(rendered(here), written(Samples.pets))
      // under Neutral the case object is the const play writes for it
      JsonSchemaProfile.Neutral.lower(here).toDebugString.replaceAll("\\s", "") should include("""const({"_type":"petshop.Fish"})""")
    }
    "a family of case objects only" in assertValid(rendered(Schemas.paint), written(Samples.paints))
    "Option fields with and without defaults" in assertValid(rendered(PlaySchemas.reading), written(Samples.readings))
    "ignore @discriminator, which play's derivation never sees (R4B)" in {
      assertValid(rendered(Schemas.shape), written(Samples.shapes))
      assertInvalid(rendered(Schemas.shape)(JsonSchemaProfile.Neutral), written(Samples.shapes)) // Neutral honors it: "kind"
    }
    "tag a member that has an implicit Schema of its own" in assertValid(rendered(Schemas.payment), written(Samples.payments))
    "value class members, written as objects" in assertValid(rendered(Schemas.badge), written(Samples.badges))
    "reject an untagged member" in assertInvalid(rendered(Schemas.pet), List("""{"name":"Rex","animal":{"name":"Rex"}}"""))
    "Map keys: negative Int and Long, Boolean and Char (a line break included) keys (#466)" in {
      written(List(Samples.charStock)) shouldBe List("""{"byChar":{"a":1,"\n":2,".":3}}""")
      assertValid(rendered(Schemas.intStock), written(List(Samples.intStock)))
      assertValid(rendered(Schemas.flagStock), written(List(Samples.flagStock)))
      assertValid(rendered(Schemas.charStock), written(List(Samples.charStock)))
      assertBadKeysRejected(rendered(_))
    }
    "Map keys: play writes a UUID-keyed Map as an array of pairs, which the object schema doesn't describe (#466)" taggedAs TriageB in {
      written(List(Samples.uuidStock)) shouldBe List("""{"byUuid":[["123e4567-e89b-12d3-a456-426614174000",1]]}""")
      pendingUntilFixed(assertValid(rendered(Schemas.uuidStock), written(List(Samples.uuidStock))))
    }
    "be the cross-check row of play's default" in {
      crossCheckRow(rendered(Schemas.pet)) shouldBe List(
        "circe semiauto"   -> 0,
        "circe configured" -> 0,
        "play default"     -> 2,
        "play _kind"       -> 0,
        "upickle"          -> 0,
        "json4s"           -> 0,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 0
      )
    }
  }

  "profiles.play._ with a JsonConfiguration of your own" should {
    import com.github.andyglow.jsonschema.profiles.play._
    import PlayCustomCodecs._

    "pick up the same implicit play's macros used (_kind, the simple-name typeNaming, WritesNull)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.playKind.mkString("[", ",", "]")) shouldBe true
      assertValid(rendered(Schemas.pet), written(Samples.pets))
      crossCheckRow(rendered(Schemas.pet)).toMap.apply("play _kind") shouldBe 2
      crossCheckRow(rendered(Schemas.pet)).toMap.apply("play default") shouldBe 0
    }
  }

  "PlayProfile" should {
    "shape a schema per call, for any later render (AsPlay._ carries no profile)" in {
      import com.github.andyglow.jsonschema.AsPlay._
      val shaped = Schemas.pet.withProfile(PlayProfile.fromConfig(PlayCustomCodecs.cfg))
      assertValid(
        byDraft(v => Json.stringify(shaped.asPlay(v)), v => Json.stringify(shaped.asPlay(v)), v => Json.stringify(shaped.asPlay(v))),
        written(Samples.pets)(PlayCustomCodecs.petF)
      )
    }
    "read play's settings" in {
      PlayProfile.default.sumStyle shouldBe JsonSchemaProfile.SumStyle.Discriminator("_type")
      PlayProfile.default.memberName shouldBe JsonSchemaProfile.MemberName.FullName
      PlayProfile.default.singletonStyle shouldBe JsonSchemaProfile.SingletonStyle.AsTagged
      PlayProfile.default.honorsSchemaAnnotations shouldBe false
      PlayProfile.fromConfig(PlayCustomCodecs.cfg).sumStyle shouldBe JsonSchemaProfile.SumStyle.Discriminator("_kind")
    }
    "fail fast on a field renaming (R6A)" in {
      val e = the[IllegalArgumentException] thrownBy PlayProfile.fromConfig(JsonConfiguration(JsonNaming.SnakeCase))
      e.getMessage should include("JsonConfiguration.naming renames fields (fooBar -> foo_bar)")
    }
  }
}
