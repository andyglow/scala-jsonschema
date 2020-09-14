package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value

import enumeratum._
import enumeratum.values._
import json.schema.{Predef => P}
import json.Schema._

trait LowPriorityEnumSupport {

  implicit def mkEnum[T <: EnumEntry](implicit co: Enum[T]): P[T] =
    P(`enum`(co.namesToValuesMap.keySet.toSet.map(Value.str)))
}

object EnumeratumSupport extends LowPriorityEnumSupport {

  implicit def mkIntEnum[T <: IntEnumEntry](implicit co: IntEnum[T]): P[T] = P(`enum`(co.valuesToEntriesMap.keySet.map(Value.num(_))))

  implicit def mkLongEnum[T <: LongEnumEntry](implicit co: LongEnum[T]): P[T] = P(`enum`(co.valuesToEntriesMap.keySet.map(Value.num(_))))

  implicit def mkShortEnum[T <: ShortEnumEntry](implicit co: ShortEnum[T]): P[T] = P(`enum`(co.valuesToEntriesMap.keySet.map(Value.num(_))))

  implicit def mkByteEnum[T <: ByteEnumEntry](implicit co: ByteEnum[T]): P[T] = P(`enum`(co.valuesToEntriesMap.keySet.map(Value.num(_))))

  implicit def mkCharEnum[T <: CharEnumEntry](implicit co: CharEnum[T]): P[T] = P(`enum`(co.valuesToEntriesMap.keySet.map(x => Value.str(x.toString))))

  implicit def mkStringEnum[T <: StringEnumEntry](implicit co: StringEnum[T]): P[T] = P(`enum`(co.valuesToEntriesMap.keySet.map(Value.str)))
}
