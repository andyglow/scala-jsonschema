package com.github.andyglow.jsonschema.refined

private[jsonschema] trait Math { this: HasLog =>

  object math {

    def min(l: Any, r: Any): Any = (l, r) match {
      case (l: Byte, r: Byte)             => l min r
      case (l: Short, r: Short)           => l min r
      case (l: Int, r: Int)               => l min r
      case (l: Long, r: Long)             => l min r
      case (l: Double, r: Double)         => l min r
      case (l: Float, r: Float)           => l min r
      case (l: BigDecimal, r: BigDecimal) => l min r
      case (l: BigInt, r: BigInt)         => l min r
      case (l: Number, r: Number)         => if (l.doubleValue() <= r.doubleValue()) l else r
      case _                              => err(s"Can't find minimal out of $l and $r")
    }

    def max(l: Any, r: Any): Any = (l, r) match {
      case (l: Byte, r: Byte)             => l max r
      case (l: Short, r: Short)           => l max r
      case (l: Int, r: Int)               => l max r
      case (l: Long, r: Long)             => l max r
      case (l: Double, r: Double)         => l max r
      case (l: Float, r: Float)           => l max r
      case (l: BigDecimal, r: BigDecimal) => l max r
      case (l: BigInt, r: BigInt)         => l max r
      case (l: Number, r: Number)         => if (l.doubleValue() >= r.doubleValue()) l else r
      case _                              => err(s"Can't find maximal out of $l and $r")
    }
  }
}
