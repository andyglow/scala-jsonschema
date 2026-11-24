package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import enumeratum._
import enumeratum.values._
import json.schema.{Predef => P}
import json.Schema._
import json.Schema.`dictionary`._

import scala.collection.immutable.ListSet

trait LowPriorityEnumSupport {

  implicit def mkEnumeratumEnum[T <: EnumEntry](implicit co: Enum[T]): P[T] =
    // declared names in declaration order, then the aliases (`extraNamesToValuesMap`, a Map) in name order
    P(`enum`(`string`, ListSet((co.values.map(_.entryName) ++ co.extraNamesToValuesMap.keys.toList.sorted).map(Value.str.apply): _*)))

  implicit def mkEnumeratumKeyPattern[T <: EnumEntry](implicit co: Enum[T]): KeyPattern[T] =
    KeyPattern.forEnum[T](co.namesToValuesMap.keySet)
}

object EnumeratumSupport extends LowPriorityEnumSupport {

  implicit def mkEnumeratumIntEnum[T <: IntEnumEntry](implicit co: IntEnum[T]): P[T] = P(
    `enum`(`integer`, ListSet(co.values.map(v => Value.num(v.value)): _*))
  )
  implicit def mkEnumeratumIntKeyPattern[T <: IntEnumEntry](implicit
    co: IntEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumLongEnum[T <: LongEnumEntry](implicit co: LongEnum[T]): P[T] = P(
    `enum`(`integer`, ListSet(co.values.map(v => Value.num(v.value)): _*))
  )
  implicit def mkEnumeratumLongKeyPattern[T <: LongEnumEntry](implicit
    co: LongEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumShortEnum[T <: ShortEnumEntry](implicit co: ShortEnum[T]): P[T] = P(
    `enum`(`integer`, ListSet(co.values.map(v => Value.num(v.value)): _*))
  )
  implicit def mkEnumeratumShortKeyPattern[T <: ShortEnumEntry](implicit
    co: ShortEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumByteEnum[T <: ByteEnumEntry](implicit co: ByteEnum[T]): P[T] = P(
    `enum`(`integer`, ListSet(co.values.map(v => Value.num(v.value)): _*))
  )
  implicit def mkEnumeratumByteKeyPattern[T <: ByteEnumEntry](implicit
    co: ByteEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumCharEnum[T <: CharEnumEntry](implicit co: CharEnum[T]): P[T] = P(
    `enum`(`string`, ListSet(co.values.map(v => Value.str(v.value.toString)): _*))
  )
  implicit def mkEnumeratumCharKeyPattern[T <: CharEnumEntry](implicit
    co: CharEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumStringEnum[T <: StringEnumEntry](implicit co: StringEnum[T]): P[T] = P(
    `enum`(`string`, ListSet(co.values.map(v => Value.str(v.value)): _*))
  )
  implicit def mkEnumeratumStringKeyPattern[T <: StringEnumEntry](implicit
    co: StringEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet)
}
