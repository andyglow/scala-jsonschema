package com.github.andyglow.json

trait ToValue[T] {

  def apply(x: T): Value
}

/** Below every other `ToValue`: `Null` conforms to `T <: Value` too, and `NullV` must win for it. */
trait LowestPriorityValueImplicits {

  /** A default of our own JSON AST type (`data: Value = Value.obj(...)`) is that value itself. */
  implicit def ValueV[T <: Value]: ToValue[T] = ToValue.mk[T](x => x)
}

trait LowPriorityPrimitiveImplicits extends LowestPriorityValueImplicits {
  import Value._
  import ToValue._

  implicit val BoolV: ToValue[Boolean]          = mk(bool.apply)
  implicit val IntV: ToValue[Int]               = mk(num.apply)
  implicit val LongV: ToValue[Long]             = mk(num.apply)
  implicit val ShortV: ToValue[Short]           = mk(num.apply)
  implicit val FloatV: ToValue[Float]           = mk(num.apply)
  implicit val DoubleV: ToValue[Double]         = mk(num.apply)
  implicit val BigDecimalV: ToValue[BigDecimal] = mk(num.apply)
  implicit val BigIntV: ToValue[BigInt]         = mk(num.apply)
  implicit val NumberV: ToValue[Number]         = mk(num.apply)
  implicit val StringV: ToValue[String]         = mk(str.apply)
  implicit val NullV: ToValue[Null]             = mk(_ => `null`)

  // Java's boxed types and java.math: exactly the Scala type's value (never `num(x: Number)`, which goes through
  // `doubleValue`). A `null` reference (a boxed field's `= null` default) is JSON `null`.
  private def boxed[T <: AnyRef](f: T => Value): ToValue[T] = mk(x => if (x == null) `null` else f(x))

  implicit val JIntegerV: ToValue[java.lang.Integer]       = boxed(x => num(x.intValue))
  implicit val JLongV: ToValue[java.lang.Long]             = boxed(x => num(x.longValue))
  implicit val JShortV: ToValue[java.lang.Short]           = boxed(x => num(x.shortValue))
  implicit val JByteV: ToValue[java.lang.Byte]             = boxed(x => num(x.byteValue))
  implicit val JFloatV: ToValue[java.lang.Float]           = boxed(x => num(x.floatValue))
  implicit val JDoubleV: ToValue[java.lang.Double]         = boxed(x => num(x.doubleValue))
  implicit val JBooleanV: ToValue[java.lang.Boolean]       = boxed(x => bool(x.booleanValue))
  implicit val JBigDecimalV: ToValue[java.math.BigDecimal] = boxed(x => num(BigDecimal(x)))
  implicit val JBigIntegerV: ToValue[java.math.BigInteger] = boxed(x => num(BigInt(x)))

  // NOTICE: there are no way to work around objects (case classes) as long as different libraries
  // may generate different json representations out of the single case class instance
}

trait LowPriorityMapImplicits { this: LowPriorityPrimitiveImplicits =>
  import Value._
  import ToValue._

  implicit def StrMapV[T](implicit to: ToValue[T]): ToValue[Map[String, T]] = {

    mk { items =>
      obj.ordered(items.toSeq map { case (k, v) => (k, to(v)) })
    }
  }

  implicit def IntMapV[T](implicit to: ToValue[T]): ToValue[Map[Int, T]] = {

    mk { items =>
      obj.ordered(items.toSeq map { case (k, v) => (k.toString, to(v)) })
    }
  }
}

trait LowPriorityProductImplicits {
  import Value._
  import ToValue._

  implicit def ProductV[T <: Product]: ToValue[T] = mk(p => str(p.productPrefix))
}
trait LowPriorityImplicits extends LowPriorityPrimitiveImplicits with LowPriorityArrayImplicits with LowPriorityMapImplicits

object ToValue extends LowPriorityImplicits {

  def mk[T](f: T => Value): ToValue[T] = new ToValue[T] { def apply(x: T): Value = f(x) }

  def apply[T](x: T)(implicit to: ToValue[T]): Value = to(x)
}
