package com.github.andyglow.json

import scala.collection.mutable
import com.github.andyglow.scalamigration._


sealed trait MutableValue {

  def toValue: Value
}

object MutableValue {

  case object `null` extends MutableValue { override def toValue = Value.`null` }

  case class bool(value: Boolean) extends MutableValue {

    override def toValue = if (value) Value.`true` else Value.`false`
  }

  case class int(value: BigInt) extends MutableValue { override def toValue = Value.num(value) }

  case class dec(value: BigDecimal) extends MutableValue { override def toValue = Value.num(value) }

  case class str(value: String) extends MutableValue { override def toValue = Value.str(value) }

  case class arr(value: mutable.ArrayBuffer[MutableValue] = mutable.ArrayBuffer.empty) extends MutableValue {

    def +=(el: MutableValue): arr = { value += el; this }

    override def toValue = Value.arr(value map { _.toValue })
  }

  case class obj(value: mutable.Map[String, MutableValue] = mutable.Map.empty) extends MutableValue {

    def update(k: String, v: MutableValue): obj = { value.update(k, v); this }

    override def toValue = Value.obj(value.toMap mapV { _.toValue })
  }
}
