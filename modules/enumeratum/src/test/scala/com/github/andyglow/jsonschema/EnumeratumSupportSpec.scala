package com.github.andyglow.jsonschema

import json._
import Schema._
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
    }

    "support EnumEntry based enums names overridden" in {
      Json.schema[E1] shouldBe `enum`.of("a", "b", "c")
    }

    "support EnumEntry based enums with *case mixed in" in {
      Json.schema[E2] shouldBe `enum`.of("aa-aa", "bb-bb", "cc-cc")
    }

    "support IntEnumEntry" in {
      Json.schema[IntV] shouldBe `enum`.of[Int](0, 1, 2)
    }

    "support LongEnumEntry" in {
      Json.schema[LongV] shouldBe `enum`.of[Long](0L, 1L, 2L)
    }

    "support ByteEnumEntry" in {
      Json.schema[ByteV] shouldBe `enum`.of[Byte](0, 1, 2)
    }

    "support ShortEnumEntry" in {
      Json.schema[ShortV] shouldBe `enum`.of[Short](0, 1, 2)
    }

    "support CharEnumEntry" in {
      Json.schema[CharV] shouldBe `enum`.of[String]("a", "b", "c")
    }

    "support StringEnumEntry" in {
      Json.schema[StringV] shouldBe `enum`.of[String]("a1", "b2", "c3")
    }

    "support StringEnumEntry with Alias enabled" in {
      Json.schema[StringVWithAlias] shouldBe `enum`.of[String]("q", "z")
    }
  }
}

object EnumeratumSupportSpec {

  sealed trait E0 extends EnumEntry
  final object E0 extends Enum[E0] {
    final case object AaAa extends E0
    final case object BbBb extends E0
    final case object CcCc extends E0
    override def values = findValues
  }

  sealed abstract class E1(override val entryName: String) extends EnumEntry
  final object E1 extends Enum[E1] {
    final case object AaAa extends E1("a")
    final case object BbBb extends E1("b")
    final case object CcCc extends E1("c")
    override def values = findValues
  }

  sealed trait E2 extends EnumEntry with Hyphencase
  final object E2 extends Enum[E2] {
    final case object AaAa extends E2
    final case object BbBb extends E2
    final case object CcCc extends E2
    override def values = findValues
  }

  sealed abstract class IntV(val value: Int) extends IntEnumEntry
  final object IntV extends IntEnum[IntV] {
    final case object AaAa extends IntV(0)
    final case object BbBb extends IntV(1)
    final case object CcCc extends IntV(2)
    override def values = findValues
  }

  sealed abstract class LongV(val value: Long) extends LongEnumEntry
  final object LongV extends LongEnum[LongV] {
    final case object AaAa extends LongV(0L)
    final case object BbBb extends LongV(1L)
    final case object CcCc extends LongV(2L)
    override def values = findValues
  }

  sealed abstract class ByteV(val value: Byte) extends ByteEnumEntry
  final object ByteV extends ByteEnum[ByteV] {
    final case object AaAa extends ByteV(0)
    final case object BbBb extends ByteV(1)
    final case object CcCc extends ByteV(2)
    override def values = findValues
  }

  sealed abstract class ShortV(val value: Short) extends ShortEnumEntry
  final object ShortV extends ShortEnum[ShortV] {
    final case object AaAa extends ShortV(0)
    final case object BbBb extends ShortV(1)
    final case object CcCc extends ShortV(2)
    override def values = findValues
  }

  sealed abstract class CharV(val value: Char) extends CharEnumEntry
  final object CharV extends CharEnum[CharV] {
    final case object AaAa extends CharV('a')
    final case object BbBb extends CharV('b')
    final case object CcCc extends CharV('c')
    override def values = findValues
  }

  sealed abstract class StringV(val value: String) extends StringEnumEntry
  final object StringV extends StringEnum[StringV] {
    final case object AaAa extends StringV("a1")
    final case object BbBb extends StringV("b2")
    final case object CcCc extends StringV("c3")
    override def values = findValues
  }

  sealed abstract class StringVWithAlias(val value: String) extends StringEnumEntry with AllowAlias
  final object StringVWithAlias extends StringEnum[StringVWithAlias] {
    final case object AaAa extends StringVWithAlias("q")
    final case object BbBb extends StringVWithAlias("q")
    final case object CcCc extends StringVWithAlias("z")
    override def values = findValues
  }
}