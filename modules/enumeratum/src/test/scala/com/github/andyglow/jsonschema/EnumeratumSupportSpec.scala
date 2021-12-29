package com.github.andyglow.jsonschema

import json._
import Schema._
import schema.validation.Instance._
import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._
import EnumeratumSupport._

class EnumeratumSupportSpec extends AnyWordSpec {
  import EnumeratumSupportSpec._

  "EnumeratumSupport" should {

    "support EnumEntry based enums" in {
      Json.schema[E0] shouldBe `enum`.of("AaAa", "BbBb", "CcCc")
      Json.schema[Map[E0, String]] shouldBe `dictionary`[E0, String, Map](`string`).withValidation(
        `patternProperties` := "^(?:AaAa|BbBb|CcCc)$"
      )
    }

    "support EnumEntry based enums names overridden" in {
      Json.schema[E1] shouldBe `enum`.of("a", "b", "c")
      Json.schema[Map[E1, String]] shouldBe `dictionary`[E1, String, Map](`string`).withValidation(
        `patternProperties` := "^(?:a|b|c)$"
      )
    }

    "support EnumEntry based enums with *case mixed in" in {
      Json.schema[E2] shouldBe `enum`.of("aa-aa", "bb-bb", "cc-cc")
      Json.schema[Map[E2, String]] shouldBe `dictionary`[E2, String, Map](`string`).withValidation(
        `patternProperties` := "^(?:aa-aa|bb-bb|cc-cc)$"
      )
    }

    "support IntEnumEntry" in {
      Json.schema[IntV] shouldBe `enum`.of[Int](0, 1, 2)
      Json.schema[Map[IntV, String]] shouldBe `dictionary`[IntV, String, Map](`string`)
        .withValidation(`patternProperties` := "^(?:0|1|2)$")
    }

    "support LongEnumEntry" in {
      Json.schema[LongV] shouldBe `enum`.of[Long](0L, 1L, 2L)
      Json.schema[Map[LongV, String]] shouldBe `dictionary`[LongV, String, Map](`string`)
        .withValidation(`patternProperties` := "^(?:0|1|2)$")
    }

    "support ByteEnumEntry" in {
      Json.schema[ByteV] shouldBe `enum`.of[Byte](0.toByte, 1.toByte, 2.toByte)
      Json.schema[Map[ByteV, String]] shouldBe `dictionary`[ByteV, String, Map](`string`)
        .withValidation(`patternProperties` := "^(?:0|1|2)$")
    }

    "support ShortEnumEntry" in {
      Json.schema[ShortV] shouldBe `enum`.of[Short](0.toShort, 1.toShort, 2.toShort)
      Json.schema[Map[ShortV, String]] shouldBe `dictionary`[ShortV, String, Map](`string`)
        .withValidation(`patternProperties` := "^(?:0|1|2)$")
    }

    "support CharEnumEntry" in {
      Json.schema[CharV] shouldBe `enum`.of[String]("a", "b", "c")
      Json.schema[Map[CharV, String]] shouldBe `dictionary`[CharV, String, Map](`string`)
        .withValidation(`patternProperties` := "^(?:a|b|c)$")
    }

    "support StringEnumEntry" in {
      Json.schema[StringV] shouldBe `enum`.of[String]("a1", "b2", "c3")
      Json.schema[Map[StringV, String]] shouldBe `dictionary`[StringV, String, Map](`string`)
        .withValidation(`patternProperties` := "^(?:a1|b2|c3)$")
    }

    "support StringEnumEntry with Alias enabled" in {
      Json.schema[StringVWithAlias] shouldBe `enum`.of[String]("q", "z")
      Json
        .schema[Map[StringVWithAlias, String]] shouldBe `dictionary`[StringVWithAlias, String, Map](
        `string`
      ).withValidation(`patternProperties` := "^(?:q|z)$")
    }
  }
}

object EnumeratumSupportSpec {

  sealed trait E0 extends EnumEntry
  object E0 extends Enum[E0] {
    case object AaAa extends E0
    case object BbBb extends E0
    case object CcCc extends E0
    override def values = findValues
  }

  sealed abstract class E1(override val entryName: String) extends EnumEntry
  object E1 extends Enum[E1] {
    case object AaAa extends E1("a")
    case object BbBb extends E1("b")
    case object CcCc extends E1("c")
    override def values = findValues
  }

  sealed trait E2 extends EnumEntry with Hyphencase
  object E2 extends Enum[E2] {
    case object AaAa extends E2
    case object BbBb extends E2
    case object CcCc extends E2
    override def values = findValues
  }

  sealed abstract class IntV(val value: Int) extends IntEnumEntry
  object IntV extends IntEnum[IntV] {
    case object AaAa extends IntV(0)
    case object BbBb extends IntV(1)
    case object CcCc extends IntV(2)
    override def values = findValues
  }

  sealed abstract class LongV(val value: Long) extends LongEnumEntry
  object LongV extends LongEnum[LongV] {
    case object AaAa extends LongV(0L)
    case object BbBb extends LongV(1L)
    case object CcCc extends LongV(2L)
    override def values = findValues
  }

  sealed abstract class ByteV(val value: Byte) extends ByteEnumEntry
  object ByteV extends ByteEnum[ByteV] {
    case object AaAa extends ByteV(0)
    case object BbBb extends ByteV(1)
    case object CcCc extends ByteV(2)
    override def values = findValues
  }

  sealed abstract class ShortV(val value: Short) extends ShortEnumEntry
  object ShortV extends ShortEnum[ShortV] {
    case object AaAa extends ShortV(0)
    case object BbBb extends ShortV(1)
    case object CcCc extends ShortV(2)
    override def values = findValues
  }

  sealed abstract class CharV(val value: Char) extends CharEnumEntry
  object CharV extends CharEnum[CharV] {
    case object AaAa extends CharV('a')
    case object BbBb extends CharV('b')
    case object CcCc extends CharV('c')
    override def values = findValues
  }

  sealed abstract class StringV(val value: String) extends StringEnumEntry
  object StringV extends StringEnum[StringV] {
    case object AaAa extends StringV("a1")
    case object BbBb extends StringV("b2")
    case object CcCc extends StringV("c3")
    override def values = findValues
  }

  sealed abstract class StringVWithAlias(val value: String) extends StringEnumEntry with AllowAlias
  object StringVWithAlias extends StringEnum[StringVWithAlias] {
    case object AaAa extends StringVWithAlias("q")
    case object BbBb extends StringVWithAlias("q")
    case object CcCc extends StringVWithAlias("z")
    override def values = findValues
  }
}
