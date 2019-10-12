package com.github.andyglow.json


trait ToValue[T] {

  def apply(x: T): Value
}

trait LowPriorityPrimitiveImplicits {
  import Value._
  import ToValue._

  implicit val BoolV: ToValue[Boolean] = mk(bool.apply)
  implicit val IntV: ToValue[Int] = mk(num.apply)
  implicit val LongV: ToValue[Long] = mk(num.apply)
  implicit val ShortV: ToValue[Short] = mk(num.apply)
  implicit val FloatV: ToValue[Float] = mk(num.apply)
  implicit val DoubleV: ToValue[Double] = mk(num.apply)
  implicit val BigDecimalV: ToValue[BigDecimal] = mk(num.apply)
  implicit val BigIntV: ToValue[BigInt] = mk(num.apply)
  implicit val NumberV: ToValue[Number] = mk(num.apply)
  implicit val StringV: ToValue[String] = mk(str.apply)
  implicit val NullV: ToValue[Null] = mk(_ => `null`)

  // NOTICE: there are no way to work around objects (case classes) as long as different libraries
  // may generate different json representations out of the single case class instance
}

trait LowPriorityMapImplicits { this: LowPriorityPrimitiveImplicits =>
  import Value._
  import ToValue._

  implicit def StrMapV[T](implicit
    to: ToValue[T]): ToValue[Map[String, T]] = {

    mk { items =>
      val v = items map { case (k, v) => (k, to(v)) }
      obj(v.toMap)
    }
  }

  implicit def IntMapV[T](implicit
    to: ToValue[T]): ToValue[Map[Int, T]] = {

    mk { items =>
      val v = items map { case (k, v) => (k.toString, to(v)) }
      obj(v.toMap)
    }
  }
}

trait LowPriorityImplicits
  extends LowPriorityPrimitiveImplicits
    with LowPriorityArrayImplicits
    with LowPriorityMapImplicits

object ToValue extends LowPriorityImplicits {

  def mk[T](f: T => Value): ToValue[T] = new ToValue[T] { def apply(x: T): Value = f(x) }

  def apply[T](x: T)(implicit to: ToValue[T]): Value = to(x)
}