package com.github.andyglow.json

import scala.collection._

sealed trait Value {

  def toString: String
}

object Value {

  case object `null` extends Value { override def toString = "null" }

  sealed abstract class bool(val value: Boolean) extends Value with Product with Serializable {

    def canEqual(that: Any): Boolean = that.isInstanceOf[bool]

    override def equals(that: Any): Boolean = canEqual(that) && (this.value == that.asInstanceOf[bool].value)

    override def hashCode: Int = value.hashCode
  }

  case object `true` extends bool(true) { override def toString = "true" }

  case object `false` extends bool(false) { override def toString = "false" }

  object bool {

    def apply(value: Boolean): bool = if (value) `true` else `false`

    def unapply(b: bool): Option[Boolean] = Some(b.value)
  }

  case class num(value: BigDecimal) extends Value { override def toString = s"$value" }

  object num {

    def apply(x: Byte): num = new num(x)

    def apply(x: Int): num = new num(x)

    def apply(x: Long): num = new num(x)

    def apply(x: Short): num = new num(x.toInt)

    def apply(x: Float): num = new num(x.toDouble)

    def apply(x: Double): num = new num(x)

    def apply(x: BigInt): num = new num(BigDecimal(x))

    def apply(x: Number): num = new num(BigDecimal(x.doubleValue()))
  }

  case class str(value: String) extends Value { override def toString = s""""$value"""" }

  case class arr(value: Seq[Value] = Seq.empty) extends Value {

    def ++(other: arr): arr = arr(value ++ other.value)

    def :+(el: Value): arr = arr(value :+ el)

    def append(el: Value): arr = this.:+(el)

    def +:(el: Value): arr = arr(el +: value)

    def prepend(el: Value): arr = this.+:(el)

    override def toString: String = value map {_.toString} mkString ("[", ", ", "]")

    def contains(that: arr): Boolean = {
      that.value.length == this.value.length &&
        that.value.forall { thatV =>
          this.value.exists { thisV =>
            (thatV, thisV) match {
              case (l: obj, r: obj) => l contains r
              case (l: arr, r: arr) => l contains r
              case _                => thisV == thatV
            }
          }
        }
    }
  }

  object arr {

    def empty: arr = arr(Seq.empty)

    def apply(x: Value, xs: Value*): arr = arr(x +: xs.toSeq)
  }

  case class obj(private val underlying: Map[String, Value]) extends Value {

    lazy val fields: Seq[(String, Value)] = underlying.toSeq

    lazy val value: Map[String, Value] = underlying match {
      case m: immutable.Map[String, value] => m
      case m                               => m.toMap
    }

    def fieldSet: Set[(String, Value)] = fields.toSet

    def keys: Set[String] = underlying.keySet

    def values: Iterable[Value] = underlying.values

    def ++(other: obj): obj = obj(underlying ++ other.underlying)

    def ++(other: Option[obj]): obj = other.fold(this)(x => obj(underlying ++ x.underlying))

    def -(otherField: String): obj = obj(underlying.toMap - otherField)

    def +(otherField: (String, Value)): obj = obj(underlying.toMap + otherField)

    def canEqual(other: Any): Boolean = other.isInstanceOf[obj]

    override def hashCode: Int = fieldSet.hashCode()

    def deepMerge(other: obj): obj = {

      def merge(existingObject: obj, otherObject: obj): obj = {
        val result = existingObject.underlying ++ otherObject.underlying.map {
          case (otherKey, otherValue) =>
            val maybeExistingValue = existingObject.underlying.get(otherKey)
            val newValue = (maybeExistingValue, otherValue) match {
              case (Some(e: obj), o: obj) => merge(e, o)
              case _                      => otherValue
            }

            (otherKey, newValue)
        }

        obj(result)
      }

      merge(this, other)
    }

    def contains(other: obj): Boolean = {
      other.underlying forall { case (k, thatV) =>
          underlying.get(k) match {
            case None        => true // ???
            case Some(thisV) =>
              (thatV, thisV) match {
                case (thisV: obj, thatV: obj) => thisV contains thatV
                case (thisV: arr, thatV: arr) => thisV contains thatV
                case _                        => thatV == thisV
              }
          }
      }
    }

    override def equals(other: Any): Boolean = other match {
      case that@obj(_) => (that canEqual this) && fieldSet == that.fieldSet
      case _           => false
    }

    override def toString = value map { case (k, v) => s""""$k": ${v.toString}"""} mkString ("{", ", ", "}")
  }

  object obj {

    sealed trait field

    object field {

      case object none extends field

      case class some(name: String, value: Value) extends field
    }

    def apply(fields: field*): obj = new obj(mutable.LinkedHashMap(fields collect {case field.some(k, v) => (k, v)}: _*))

    val empty: obj = obj()
  }

  sealed trait FieldAdapter[T] {

    def adapt(x: (String, T)): obj.field
  }

  sealed trait ValueAdapter[T] {

    type J <: Value

    def adapt(x: T): J
  }

  trait LowPriorityValueAdapters {

    implicit object booleanAdapter extends ValueAdapter[Boolean] {
      type J = bool
      def adapt(x: Boolean): bool = bool(x)
    }

    implicit object stringAdapter extends ValueAdapter[String] {
      type J = str
      def adapt(x: String): str = str(x)
    }

    implicit def numberAdapter[T](implicit n: Numeric[T]): ValueAdapter[T] = new ValueAdapter[T] {
      type J = num
      def adapt(x: T): num = x match {
        case x: Int => num(x)
        case _      => num(n.toDouble(x))
      }
    }
  }

  trait LowPriorityFieldAdapters {

    implicit def tupleAdapter[T](implicit a: ValueAdapter[T]): FieldAdapter[T] = new FieldAdapter[T] {

      def adapt(x: (String, T)): obj.field = {
        val (k, v) = x

        obj.field.some(k, a adapt v)
      }
    }

    implicit def optTupleAdapter[T](implicit a: ValueAdapter[T]): FieldAdapter[Option[T]] = new FieldAdapter[Option[T]] {

      def adapt(x: (String, Option[T])): obj.field = x._2 match {
        case Some(v) => obj.field.some(x._1, a.adapt(v))
        case None    => obj.field.none
      }
    }
  }

  object ValueAdapter extends LowPriorityValueAdapters

  object FieldAdapter extends LowPriorityFieldAdapters

  import scala.language.implicitConversions

  implicit def convertValue[T](x: T)(implicit adapter: ValueAdapter[T]): adapter.J = adapter adapt x

  implicit def convertField[T](x: (String, T))(implicit adapter: FieldAdapter[T]): obj.field = adapter adapt x

  implicit def convertValueField[T](x: (String, Value)): obj.field = obj.field.some(x._1, x._2)

  implicit def convertOptValueField[T](x: (String, Option[Value])): obj.field = x match {
    case (f, Some(v)) => obj.field.some(f, v)
    case (_, None)    => obj.field.none
  }

  implicit class IterableOps[T](val x: Iterable[T]) {

    def toArr(implicit elementAdapter: ValueAdapter[T]): arr = arr(x.map { elementAdapter adapt _ }.toSeq)
  }

  implicit val valueOrdering: Ordering[Value] = {
    Ordering fromLessThan {
      case (l: str , r: str)   => l.value < r.value
      case (l: num , r: num)   => l.value < r.value
      case (l: bool, r: bool)  => l.value < r.value
      case (l: arr , r: arr)   => if (l.value.length == r.value.length)
                                    l.value.zip(r.value).forall { case (l, r) => valueOrdering.lt(l, r) }
                                  else
                                    l.value.length < r.value.length
      case (l: obj , r: obj)   => if (l.value.keySet == r.value.keySet) {
                                    val zipped = l.value.foldLeft[List[(Value, Value)]](Nil) { case (acc, (field, ll)) =>
                                      val rr = r.value(field)
                                      acc :+ ((ll, rr))
                                    }
                                    zipped.forall { case (l, r) => valueOrdering.lt(l, r) }
                                  } else
                                    l.value.keySet.toSeq.sorted.mkString < r.value.keySet.toSeq.sorted.mkString
      case (`null` , _)        => true
      case (l, r)              => def weight(x: Value): Int = x match {
                                    case `null`  => 100
                                    case _: bool => 50
                                    case _: num  => 40
                                    case _: str  => 30
                                    case _: arr  => 20
                                    case _: obj  => 10
                                  }
                                  weight(l) < weight(r)
    }
  }
}