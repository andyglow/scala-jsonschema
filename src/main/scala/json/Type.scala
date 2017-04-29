package json

sealed trait Type extends Product
object Type {
  case object `boolean` extends Type
  case class `number`(format: String) extends Type
  case class `string`(format: Option[String], pattern: Option[String]) extends Type
  case class `array`(schema: Schema) extends Type
  case class `object`(fields: Set[`object`.Field]) extends Type
  object `object` {
    case class Field(name: String, schema: Schema, required: Boolean = true) {
      def canEqual(that: Any): Boolean = that.isInstanceOf[Field]
      override def equals(that: Any): Boolean = canEqual(that) && (this.name == that.asInstanceOf[Field].name)
      override def hashCode: Int = name.hashCode
    }
    def apply(fields: Field*): `object` = new `object`(fields.toSet)
  }
}




