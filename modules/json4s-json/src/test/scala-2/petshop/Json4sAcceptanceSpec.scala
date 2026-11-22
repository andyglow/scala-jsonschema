package petshop

import json.Schema
import json.schema.{JsonSchemaProfile, Version}
import org.json4s.{DefaultFormats, Formats, ShortTypeHints}
import org.json4s.native.{JsonMethods, Serialization}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

object Json4sSchemas {
  import com.github.andyglow.jsonschema.AsJson4s.toValue // the Some(1) default's value, from json4s' Writer[Option[Int]]
  import org.json4s.DefaultWriters._
  val reading: Schema[Reading] = json.Json.schema[Reading]
}

/** Scala 2 only (json4s 4 can't serialize case classes on Scala 3: no ScalaSig): what `Serialization.write` writes, validated against the schema under the `Formats`' profile. */
class Json4sAcceptanceSpec extends AnyWordSpec {
  import Acceptance._

  // the schema as `asJson4s` renders it under the profile in scope where this is called
  private def rendered(s: Schema[_])(implicit profile: JsonSchemaProfile): Version => String = {
    import com.github.andyglow.jsonschema.AsJson4s._
    def r(j: org.json4s.JValue) = JsonMethods.compact(JsonMethods.render(j))
    byDraft(v => r(s.asJson4s(v)), v => r(s.asJson4s(v)), v => r(s.asJson4s(v)))
  }
  private def written[T <: AnyRef](xs: List[T])(implicit f: Formats): List[String] = xs.map(x => Serialization.write(x))

  "profiles.json4s._ with DefaultFormats" should {
    import com.github.andyglow.jsonschema.profiles.json4s._
    implicit val formats: Formats = DefaultFormats

    "describe what Serialization.write writes (untagged members, a case object as {}, None omitted)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.json4s.mkString("[", ",", "]")) shouldBe true
      assertValid(rendered(Schemas.pet), written(Samples.pets))
    }
    "render with the import's own asJson4s" in {
      JsonMethods.compact(JsonMethods.render(Schemas.pet.asJson4s(Version.Draft12("x")))) shouldBe rendered(Schemas.pet).apply(Version.Draft12("x"))
    }
    "a nested family" in assertValid(rendered(Schemas.garage), written(List(Samples.garage)))
    "a family of case objects only" in assertValid(rendered(Schemas.paint), written(Samples.paints))
    "Option fields with defaults" in assertValid(rendered(Json4sSchemas.reading), written(Samples.readings))
    "ignore @discriminator (R4B)" in assertValid(rendered(Schemas.shape), written(Samples.shapes))
    "a member that has an implicit Schema of its own" in assertValid(rendered(Schemas.payment), written(Samples.payments))
    "value class members" in assertValid(rendered(Schemas.badge), written(Samples.badges))
    "be the cross-check row of json4s" in {
      crossCheckRow(rendered(Schemas.pet)) shouldBe List(
        "circe semiauto"   -> 0,
        "circe configured" -> 0,
        "play default"     -> 0,
        "play _kind"       -> 0,
        "upickle"          -> 0,
        "json4s"           -> 2,
        "json4s hints"     -> 0,
        "spray (AnimalF)"  -> 0
      )
    }
  }

  "profiles.json4s._ with type hints" should {
    import com.github.andyglow.jsonschema.profiles.json4s._
    implicit val formats: Formats = DefaultFormats + ShortTypeHints(List(classOf[Dog], Fish.getClass, classOf[Sedan], Coupe.getClass, classOf[Bike]), "kind")

    "describe what Serialization.write writes (the hint field per class, hintFor the value)" in {
      sameJson(written(Samples.pets).mkString("[", ",", "]"), LibraryOutputs.json4sHints.mkString("[", ",", "]")) shouldBe true
      assertValid(rendered(Schemas.pet), written(Samples.pets))
      crossCheckRow(rendered(Schemas.pet)).toMap.apply("json4s hints") shouldBe 2
      crossCheckRow(rendered(Schemas.pet)).toMap.apply("json4s") shouldBe 0
    }
    "a nested family, its leaves hinted" in assertValid(rendered(Schemas.garage), written(List(Samples.garage)))
    "a family without hints, untagged next to the hinted ones" in assertValid(rendered(Schemas.paint), written(Samples.paints))
  }
}
