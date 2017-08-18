package json

import scala.collection._

sealed trait Value {

  def rendered: String
}

object Value {

  case object `null` extends Value { def rendered = "null" }

  sealed abstract class bool(val value: Boolean) extends Value with Product with Serializable {

    def canEqual(that: Any): Boolean = that.isInstanceOf[bool]

    override def equals(that: Any): Boolean = canEqual(that) && (this.value == that.asInstanceOf[bool].value)

    override def hashCode: Int = value.hashCode
  }

  case object `true` extends bool(true) { def rendered = "true" }

  case object `false` extends bool(false) { def rendered = "false" }

  object bool {

    def apply(value: Boolean): bool = if (value) `true` else `false`

    def unapply(b: bool): Option[Boolean] = Some(b.value)
  }

  case class num(value: BigDecimal) extends Value { def rendered = s"$value" }

  case class str(value: String) extends Value { def rendered = s""""$value"""" }

  case class arr(value: Seq[Value] = Seq.empty) extends Value {

    def ++(other: arr): arr = arr(value ++ other.value)

    def :+(el: Value): arr = arr(value :+ el)

    def append(el: Value): arr = this.:+(el)

    def +:(el: Value): arr = arr(el +: value)

    def prepend(el: Value): arr = this.+:(el)

    def rendered: String = value map {_.rendered} mkString ("[", ", ", "]")
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

    def -(otherField: String): obj = obj(underlying - otherField)

    def +(otherField: (String, obj)): obj = obj(underlying + otherField)

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

            otherKey -> newValue
        }

        obj(result)
      }

      merge(this, other)
    }

    override def equals(other: Any): Boolean = other match {
      case that@obj(_) => (that canEqual this) && fieldSet == that.fieldSet
      case _           => false
    }

    def rendered = value map {x => s""""${x._1}": ${x._2.rendered}"""} mkString ("{", ", ", "}")
  }

  object obj {

    sealed trait field

    object field {

      case object none extends field

      case class some(name: String, value: Value) extends field
    }

    def apply(fields: field*): obj = new obj(mutable.LinkedHashMap(fields collect {case field.some(k, v) => (k, v)}: _*))

    def empty: obj = obj()
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
      def adapt(x: T): num = num(n.toDouble(x))
    }

    implicit def primitiveAdapter[T]: ValueAdapter[T] = new ValueAdapter[T] {
      type J = Value
      def adapt(x: T): Value = x match {
        case x: String => str(x)
        case x: Boolean => bool(x)
        case x: Int => num(x)
        case x: Long => num(x)
        case x: Double => num(x)
        case x: Float => num(x.toDouble)
        case x: Short => num(x.toInt)
        case x: BigInt => num(x.toInt)
        case x: BigDecimal => num(x)
        case _ => sys.error(s"type ${x.getClass.getName} is not supported")
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
}