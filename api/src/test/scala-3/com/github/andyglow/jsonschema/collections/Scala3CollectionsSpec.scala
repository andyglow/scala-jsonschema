package com.github.andyglow.jsonschema.collections

import json.*
import json.Schema.*
import json.Schema.`dictionary`.KeyPattern
import json.Schema.`object`.Field
import json.schema.validation.Instance.`patternProperties`
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

object Scala3CollectionsSpec {
  case class User(name: String)
  case class Item(id: Int) derives json.Schema

  type MyList[A] = List[A]
  case class G[A](a: A)
  type GInt[A] = G[A]

  case class Spelled(m: Map[String, scala.collection.immutable.List[User]])

  case class Room(id: String)
  object RoomKeys { given KeyPattern[Room] = KeyPattern.mk[Room]("^room-.*$") }

  class UserMap extends Map[String, User] {
    def removed(key: String): Map[String, User]                      = this
    def updated[V1 >: User](key: String, value: V1): Map[String, V1] = this
    def get(key: String): Option[User]                               = None
    def iterator: Iterator[(String, User)]                           = Iterator.empty
  }
}

/** Collections and dictionaries: cases Scala 3 adds or handles differently from Scala 2. The shared cases are in [[CollectionsSpec]]. */
class Scala3CollectionsSpec extends AnyFunSuite {
  import Scala3CollectionsSpec.*

  private val user = `object`[User](Field("name", `string`))

  test("a `derives json.Schema` element is a definition (it is a user-provided implicit Schema)") {
    val item = `def`[Item]("com.github.andyglow.jsonschema.collections.Scala3CollectionsSpec.Item", `object`[Item](Field("id", `integer`)))
    Json.schema[List[Item]] shouldEqual `array`[Item, List](item)
    Json.schema[Map[Int, Item]] shouldEqual `dictionary`[Int, Item, Map](item).withValidation(`patternProperties` := "^[0-9]+$")
  }

  test("a `given` KeyPattern is used for the key") {
    import RoomKeys.given
    Json.schema[Map[Room, User]] shouldEqual `dictionary`[Room, User, Map](user).withValidation(`patternProperties` := "^room-.*$")
  }

  test("Json.sig of a user-defined eta-reducible alias: Scala 3 has already reduced it (Scala 2 prints the alias)") {
    // Scala 2: ...Scala3CollectionsSpec.MyList[scala.Int] and ...Scala3CollectionsSpec.GInt[scala.Int]
    Json.sig[MyList[Int]].signature shouldEqual "scala.List[scala.Int]"
    Json.sig[GInt[Int]].signature shouldEqual "com.github.andyglow.jsonschema.collections.Scala3CollectionsSpec.G[scala.Int]"
  }

  test("a definition name maps standard classes in type arguments, also when spelled out (Scala 2 keeps the class name there)") {
    // the alias signal Json.sig uses isn't in TASTy, so definition names always map (reviewer4 B1); Scala 2: ...Map[scala.Predef.String,scala.collection.immutable.List[...User]]
    given Schema[Map[String, scala.collection.immutable.List[User]]] = `dictionary`[String, List[User], Map](`array`[User, List](user))
    Json.schema[Spelled] shouldEqual `object`[Spelled](
      Field(
        "m",
        `def`[Map[String, List[User]]](
          "scala.collection.immutable.Map[scala.Predef.String,scala.List[com.github.andyglow.jsonschema.collections.Scala3CollectionsSpec.User]]",
          `dictionary`[String, List[User], Map](`array`[User, List](user))
        )
      )
    )
    // Json.sig keeps the spelling, as on Scala 2
    Json.sig[Map[String, scala.collection.immutable.List[User]]].signature shouldEqual
      "scala.Predef.Map[scala.Predef.String,scala.collection.immutable.List[com.github.andyglow.jsonschema.collections.Scala3CollectionsSpec.User]]"
  }

  test("a user's Map class that doesn't take its key and value types as its type parameters: a clear compile error (shared cases: IntMap, LongMap in CollectionsSpec)") {
    val err = scala.compiletime.testing.typeCheckErrors("Json.schema[UserMap]").map(_.message).mkString("\n")
    err should include("is not supported")
    err should include("UserMap")
    err should include("a Map must take its key and value types")
  }
}
