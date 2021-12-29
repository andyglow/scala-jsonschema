package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import enumeratum._
import enumeratum.values._
import json.schema.{Predef => P}
import json.Schema._
import json.Schema.`dictionary`._

trait LowPriorityEnumSupport {

  implicit def mkEnumeratumEnum[T <: EnumEntry](implicit co: Enum[T]): P[T] =
    P(`enum`(`string`, co.namesToValuesMap.keySet.map(Value.str)))

  implicit def mkEnumeratumKeyPattern[T <: EnumEntry](implicit co: Enum[T]): KeyPattern[T] =
    KeyPattern.forEnum[T](co.namesToValuesMap.keySet)
}

object EnumeratumSupport extends LowPriorityEnumSupport {

  implicit def mkEnumeratumIntEnum[T <: IntEnumEntry](implicit co: IntEnum[T]): P[T] = P(
    `enum`(`integer`, co.valuesToEntriesMap.keySet.map(Value.num(_)))
  )
  implicit def mkEnumeratumIntKeyPattern[T <: IntEnumEntry](implicit
      co: IntEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumLongEnum[T <: LongEnumEntry](implicit co: LongEnum[T]): P[T] = P(
    `enum`(`integer`, co.valuesToEntriesMap.keySet.map(Value.num(_)))
  )
  implicit def mkEnumeratumLongKeyPattern[T <: LongEnumEntry](implicit
      co: LongEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumShortEnum[T <: ShortEnumEntry](implicit co: ShortEnum[T]): P[T] = P(
    `enum`(`integer`, co.valuesToEntriesMap.keySet.map(Value.num(_)))
  )
  implicit def mkEnumeratumShortKeyPattern[T <: ShortEnumEntry](implicit
      co: ShortEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumByteEnum[T <: ByteEnumEntry](implicit co: ByteEnum[T]): P[T] = P(
    `enum`(`integer`, co.valuesToEntriesMap.keySet.map(Value.num(_)))
  )
  implicit def mkEnumeratumByteKeyPattern[T <: ByteEnumEntry](implicit
      co: ByteEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumCharEnum[T <: CharEnumEntry](implicit co: CharEnum[T]): P[T] = P(
    `enum`(`string`, co.valuesToEntriesMap.keySet.map(x => Value.str(x.toString)))
  )
  implicit def mkEnumeratumCharKeyPattern[T <: CharEnumEntry](implicit
      co: CharEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet map { _.toString })

  implicit def mkEnumeratumStringEnum[T <: StringEnumEntry](implicit co: StringEnum[T]): P[T] = P(
    `enum`(`string`, co.valuesToEntriesMap.keySet.map(Value.str))
  )
  implicit def mkEnumeratumStringKeyPattern[T <: StringEnumEntry](implicit
      co: StringEnum[T]
  ): KeyPattern[T] = KeyPattern.forEnum[T](co.valuesToEntriesMap.keySet)
}
