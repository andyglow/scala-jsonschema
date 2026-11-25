package com.github.andyglow.jsonschema.collections

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.Schema._
import json.Schema.`dictionary`.KeyPattern
import json.Schema.`object`.Field
import json.schema.validation.Instance.`patternProperties`
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.{ListMap, ListSet, SortedMap}

object CollectionsSpec {
  case class User(name: String)

  case class Arrays(
    list: List[User],
    vector: Vector[User],
    seq: Seq[User],
    indexedSeq: IndexedSeq[User],
    iterable: Iterable[User],
    set: Set[User],
    listSet: ListSet[User],
    mutableSet: scala.collection.mutable.Set[User],
    array: Array[User],
    nested: List[Set[User]]
  )

  case class StrId(value: String)            extends AnyVal
  case class JStrId(value: java.lang.String) extends AnyVal
  case class IntId(value: Int)               extends AnyVal

  case class Dicts(
    str: Map[String, User],
    javaStr: Map[java.lang.String, User],
    int: Map[Int, User],
    long: Map[Long, User],
    short: Map[Short, User],
    byte: Map[Byte, User],
    char: Map[Char, User],
    strId: Map[StrId, User],
    jStrId: Map[JStrId, User],
    intId: Map[IntId, User],
    listMap: ListMap[String, User],
    sorted: SortedMap[String, User],
    nested: Map[String, Map[Int, List[User]]]
  )

  case class UuidKeys(m: Map[java.util.UUID, User])
  object UuidKeyPattern { implicit val uuidKeys: KeyPattern[java.util.UUID] = KeyPattern.mk[java.util.UUID]("^[0-9a-f-]{36}$") }

  case class Tree(value: Int, children: List[Tree])
  case class Forest(trees: Map[String, Tree])

  object UserSchema { implicit val userSchema: Schema[User] = Json.schema[User]                                                         }
  object UserPredef { implicit val userPredef: json.schema.Predef[User] = json.schema.Predef(`object`[User](Field("custom", `string`))) }

  type Users     = List[User]
  type UserMap   = Map[String, User]
  type StrMap[V] = Map[String, V]
  case class Box[A](a: A)
  case class HasAliases(users: Users, byName: UserMap)

  case class HasUserSchemas(users: List[User], byName: Map[String, List[User]], nested: List[Vector[User]])
  // implicit Schemas for HasUserSchemas' fields, so they are definitions named after their types; also used from another module (spray-json's CrossModuleCollectionsSpec)
  object UserSchemas {
    private val user                                     = `object`[User](Field("name", `string`))
    implicit val users: Schema[List[User]]               = `array`[User, List](user)
    implicit val byName: Schema[Map[String, List[User]]] = `dictionary`[String, List[User], Map](`array`[User, List](user))
    implicit val nestedUsers: Schema[List[Vector[User]]] = `array`[Vector[User], List](`array`[User, Vector](user))
  }

  case class HasBitSet(b: scala.collection.immutable.BitSet)
  class Path[A] extends Iterable[String] { def iterator: Iterator[String] = Iterator.empty }
  case class HasPath(p: Path[Int])
  case class HasMutableMap(m: scala.collection.mutable.Map[String, User])
  case class HasCollectionMap(m: scala.collection.Map[String, User])
  case class HasDateKeys(m: Map[java.time.LocalDate, User]) // no built-in KeyPattern (UUID has one since #466)
  case class DoubleKeys(m: Map[Double, User])
  case class SortedDicts(sorted: SortedMap[Int, User], listMap: ListMap[Long, User], byChar: SortedMap[Char, User])
  // Map subtypes that don't take the key and value types as their only type parameters
  case class HasIntMap(m: scala.collection.immutable.IntMap[User])
  case class HasLongMap(m: scala.collection.immutable.LongMap[User])
}

/** Collections and dictionaries of derived elements (Scala 2's `UArrays`/`UDictionaries`, the Scala 3 port's stream 3). Elements of built-in types come from the `Predef` instances (`listS`, `dictS`,
  * ...) and aren't derived; these tests use a derived element (`User`) so the derivation itself is exercised. Goldens are Scala 2's output.
  */
class CollectionsSpec extends AnyFunSuite {
  import CollectionsSpec._
  import Golden.check
  import ErrorPhrases._

  private val user = `object`[User](Field("name", `string`))

  test("arrays of derived elements: unique iff immutable.Set") {
    Json.schema[List[User]] shouldEqual `array`[User, List](user)
    Json.schema[Vector[User]] shouldEqual `array`[User, Vector](user)
    Json.schema[Set[User]] shouldEqual `array`[User, Set](user, unique = true)
    Json.schema[ListSet[User]] shouldEqual `array`[User, ListSet](user, unique = true)
    Json.schema[scala.collection.mutable.Set[User]] shouldEqual `array`[User, scala.collection.mutable.Set](user)
    Json.schema[Array[User]] shouldEqual `array`[User, Array](user)
    check("collections/Arrays", Json.schema[Arrays])
  }

  test("dictionaries of derived values: key patterns") {
    Json.schema[Map[String, User]] shouldEqual `dictionary`[String, User, Map](user)
    // quirk 13: java.lang.String isn't the alias scala.Predef.String, so its KeyPattern (any key, not enforced) applies
    Json.schema[Map[java.lang.String, User]] shouldEqual `dictionary`[String, User, Map](user).withValidation(`patternProperties` := "^[\\s\\S]*$")
    Json.schema[Map[Int, User]] shouldEqual `dictionary`[Int, User, Map](user).withValidation(`patternProperties` := "^-?[0-9]+$")
    Json.schema[Map[Char, User]] shouldEqual `dictionary`[Char, User, Map](user).withValidation(`patternProperties` := "^[\\s\\S]$")
    Json.schema[Map[StrId, User]] shouldEqual `dictionary`[StrId, User, Map](user)
    Json.schema[Map[IntId, User]] shouldEqual `dictionary`[IntId, User, Map](user).withValidation(`patternProperties` := "^-?[0-9]+$")
    check("collections/Dicts", Json.schema[Dicts])
  }

  test("a Map subtype other than Map itself with a key pattern (0.7: an error in the generated code, no validation.Magnet)") {
    Json.schema[SortedMap[Int, User]] shouldEqual `dictionary`[Int, User, SortedMap](user).withValidation[Map[Int, User], Map[_, _]](`patternProperties` := "^-?[0-9]+$")
    Json.schema[ListMap[Long, User]] shouldEqual `dictionary`[Long, User, ListMap](user).withValidation[Map[Long, User], Map[_, _]](`patternProperties` := "^-?[0-9]+$")
    check("collections/SortedDicts", Json.schema[SortedDicts])
  }

  test("a user-provided KeyPattern makes any key type usable") {
    import UuidKeyPattern._
    Json.schema[Map[java.util.UUID, User]] shouldEqual `dictionary`[java.util.UUID, User, Map](user).withValidation(`patternProperties` := "^[0-9a-f-]{36}$")
    check("collections/UuidKeys", Json.schema[UuidKeys])
  }

  test("elements and values follow the core rule: an implicit Schema is a definition, an implicit Predef is inlined") {
    locally {
      import UserSchema._
      val d = `def`[User]("com.github.andyglow.jsonschema.collections.CollectionsSpec.User", user)
      Json.schema[List[User]] shouldEqual `array`[User, List](d)
      Json.schema[Map[Int, User]] shouldEqual `dictionary`[Int, User, Map](d).withValidation(`patternProperties` := "^-?[0-9]+$")
      check("collections/Arrays.implicitSchema", Json.schema[Arrays])
    }
    locally {
      import UserPredef._
      // List[User] and Map[String, User] now come from Predef.listS / dictS
      Json.schema[List[User]] shouldEqual `array`[User, List](`object`[User](Field("custom", `string`)))
      check("collections/Dicts.implicitPredef", Json.schema[Dicts])
    }
  }

  test("with Json.auto imported, collections derive as without it") {
    val plain = (Json.schema[List[User]], Json.schema[Map[Int, User]], Json.schema[Arrays], Json.schema[Dicts])
    locally {
      import Json.auto._
      (Json.schema[List[User]], Json.schema[Map[Int, User]], Json.schema[Arrays], Json.schema[Dicts]) shouldEqual plain
      implicitly[json.schema.Predef[List[User]]].schema shouldEqual plain._1
    }
  }

  test("recursion through collections") {
    check("collections/Tree", Json.schema[Tree])
    check("collections/Forest", Json.schema[Forest])
  }

  test("parameterless collection aliases, as root and as fields") {
    Json.schema[Users] shouldEqual `array`[User, List](user)
    Json.schema[UserMap] shouldEqual `dictionary`[String, User, Map](user)
    check("collections/HasAliases", Json.schema[HasAliases])
  }

  test("signatures of collections (Json.sig and definition names)") {
    Json.sig[List[User]].signature shouldEqual "scala.List[com.github.andyglow.jsonschema.collections.CollectionsSpec.User]"
    Json.sig[Map[String, List[Int]]].signature shouldEqual "scala.Predef.Map[scala.Predef.String,scala.List[scala.Int]]"
    Json.sig[Set[Vector[Int]]].signature shouldEqual "scala.Predef.Set[scala.Vector[scala.Int]]"
    Json.sig[Seq[Int]].signature shouldEqual "scala.Seq[scala.Int]"
    Json.sig[Users].signature shouldEqual "com.github.andyglow.jsonschema.collections.CollectionsSpec.Users"
    Json.sig[IndexedSeq[Int]].signature shouldEqual "scala.IndexedSeq[scala.Int]"
    Json.sig[Iterable[Int]].signature shouldEqual "scala.Iterable[scala.Int]"
    Json.sig[Array[Int]].signature shouldEqual "scala.Array[scala.Int]"
    Json.sig[Either[Int, String]].signature shouldEqual "scala.Either[scala.Int,scala.Predef.String]"
    Json.sig[StrMap[Int]].signature shouldEqual "com.github.andyglow.jsonschema.collections.CollectionsSpec.StrMap[scala.Int]"
    Json.sig[Box[Users]].signature shouldEqual "com.github.andyglow.jsonschema.collections.CollectionsSpec.Box[com.github.andyglow.jsonschema.collections.CollectionsSpec.Users]"
    // spelled out (or imported) classes keep their class name
    Json.sig[scala.collection.immutable.List[Int]].signature shouldEqual "scala.collection.immutable.List[scala.Int]"
    Json.sig[scala.collection.immutable.Map[String, Int]].signature shouldEqual "scala.collection.immutable.Map[scala.Predef.String,scala.Int]"
    Json.sig[scala.collection.Seq[Int]].signature shouldEqual "scala.collection.Seq[scala.Int]"
    Json.sig[scala.collection.mutable.Map[String, Int]].signature shouldEqual "scala.collection.mutable.Map[scala.Predef.String,scala.Int]"
    Json.sig[scala.util.Either[Int, String]].signature shouldEqual "scala.util.Either[scala.Int,scala.Predef.String]"
    locally {
      import scala.collection.immutable.Map
      Json.sig[Map[String, Int]].signature shouldEqual "scala.collection.immutable.Map[scala.Predef.String,scala.Int]"
      Json.sig[Box[Map[String, Int]]].signature shouldEqual "com.github.andyglow.jsonschema.collections.CollectionsSpec.Box[scala.collection.immutable.Map[scala.Predef.String,scala.Int]]"
    }
    // a definition is named after the dealiased type: the class at the top level, type arguments as written
    locally {
      import UserSchemas._
      check("collections/HasUserSchemas", Json.schema[HasUserSchemas])
    }
  }

  test("unsupported collections and keys: clear compile errors naming the type") {
    val bitSet = errorOf(assertCompiles("Json.schema[HasBitSet]"))
    bitSet should include(NotSupported)
    bitSet should include("BitSet")
    val path = errorOf(assertCompiles("Json.schema[HasPath]"))
    path should include(NotSupported)
    path should include("Path")
    val mutableMap = errorOf(assertCompiles("Json.schema[HasMutableMap]"))
    mutableMap should include(NotSupported)
    mutableMap should include("only immutable Maps")
    errorOf(assertCompiles("Json.schema[HasCollectionMap]")) should include("only immutable Maps")
    val date = errorOf(assertCompiles("Json.schema[HasDateKeys]"))
    date should include(NotSupported)
    date should include("LocalDate")
    date should include("a Map key must be")
    val double = errorOf(assertCompiles("Json.schema[DoubleKeys]"))
    double should include("a Map key must be")
    double should include("Double")
    // 0.7: a crash (typeArgs.head) or an error in the generated code
    val intMap = errorOf(assertCompiles("Json.schema[HasIntMap]"))
    intMap should include(NotSupported)
    intMap should include("IntMap")
    intMap should include("a Map must take its key and value types as its only type parameters")
    val longMap = errorOf(assertCompiles("Json.schema[HasLongMap]"))
    longMap should include("LongMap")
    longMap should include("a Map must take its key and value types as its only type parameters")
  }
}
