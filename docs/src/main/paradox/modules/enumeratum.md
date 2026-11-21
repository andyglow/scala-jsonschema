# Enumeratum

```scala mdoc:compile-only
import json._
import com.github.andyglow.jsonschema.EnumeratumSupport._
import enumeratum._
import enumeratum.values._

sealed trait Size extends EnumEntry
object Size extends Enum[Size] {
  case object Small extends Size
  case object Large extends Size
  val values = findValues
}

sealed abstract class Priority(val value: Int) extends IntEnumEntry
object Priority extends IntEnum[Priority] {
  case object Low extends Priority(1)
  case object High extends Priority(10)
  val values = findValues
}

case class Order(size: Size, priority: Priority)

Json.schema[Order] // size: {"type": "string", "enum": ["Small", "Large"]}, priority: {"type": "integer", "enum": [1, 10]}
```

`Enum` entries give a string enum of their names. `IntEnum`, `LongEnum`, `ShortEnum`, `ByteEnum`, `StringEnum` and
`CharEnum` give an enum of their values. They work as `Map` keys too.
