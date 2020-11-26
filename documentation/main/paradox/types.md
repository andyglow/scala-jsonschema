# Types

`scala-jsonschema` has ben designed to be pretty flexible on how jsonschema is exposed from type.
So there is a list of types supported out of the box.    

- `Boolean`
- `String`, `Char`
- Numeric: `Short`, `Int`, `Double`, `Float`, `Long`, `BigInt`, `BigDecimal`
- Date Time: `java.util.Date`, `java.sql.Date`, `java.sql.Time`, `java.sql.Timestamp`, `java.time.Instant`, `java.time.LocalDate`, `java.time.LocalTime`, `java.time.LocalDateTime`
- Misc:, `java.util.UUID`, `java.net.URL`, `java.net.URI`
- Collections
    - Dictionary (eg. `Map[String, T]`, `Map[Int, T]`, `Map[K, V]`, etc)
    - `Iterable[T]`
- Sum types 
    - Sealed Trait hierarchy of case objects (enum)
    - Sealed Trait hierarchy of case classes (oneof)
- Product types - Case Classes
- Value Classes

Support for some other types can be brought on a table by using provided extension modules. 
For the rest please look into the customization section.

## Primitives
For primitives types there is a mapping between type and it's json-schema representation

| Type               | JsonSchema                                       |
| :----------------- | :----------------------------------------------- |
| `Boolean`          | `{ type: "boolean" }`                            |
| `String`           | `{ type: "string" }`                             |
| `Char`             | `{ type: "string", minLength: 1, maxLength: 1 }` |
| `Byte`             | `{ type: "number" }`                             |
| `Short`            | `{ type: "number" }`                             |
| `Int`              | `{ type: "integer" }`                            |
| `Float`            | `{ type: "number" }`                             |
| `Double`           | `{ type: "number" }`                             |
| `Long`             | `{ type: "number" }`                             |
| `BigInt`           | `{ type: "number" }`                             |
| `BigDecimal`       | `{ type: "number" }`                             |
| `ju.UUID`          | `{ type: "string", pattern: "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$" }` |
| `jn.URI`           | `{ type: "string", format: "uri" }`              |
| `jn.URL`           | `{ type: "string", format: "uri" }`              |
| `ju.Date`          | `{ type: "string", format: "date-time" }`        |
| `js.Date`          | `{ type: "string", format: "date" }`             |
| `js.Time`          | `{ type: "string", format: "time" }`             |
| `js.Timestamp`     | `{ type: "string", format: "date-time" }`        |
| `jt.Instant`       | `{ type: "string", format: "date-time" }`        |
| `jt.LocalDate`     | `{ type: "string", format: "date" }`             |
| `jt.LocalTime`     | `{ type: "string", format: "time" }`             |
| `jt.LocalDateTime` | `{ type: "string", format: "date-time" }`        |

## Product types  
In scala product types represented by case classes.
`scala-jsonschema` maps `scala` `case class` to `jsonschema` `object` with case class fields mapped to `jsonschema` `properties`.

```scala mdoc
case class CaseClass(field1: String, field2: Boolean, field3: Option[BigInt])
```
is mapped to
```scala mdoc
import json._
import com.github.andyglow.jsonschema._

Json.schema[CaseClass].stringify
```

## Sum types
Sum Types in scala are represented by sealed trait hierarchies.
`scala-jsonschema` supports 2 cases

### Hierarchy of case object is mapped to `enum`
```scala mdoc
sealed trait Color
object Color {
    case object Red extends Color
    case object Blue extends Color
    case object Green extends Color
}
```
is mapped to
```scala mdoc
Json.schema[Color].stringify
```

### Hierarchy of case classes is mapped to `oneof`
```scala mdoc
sealed trait Result
object Result {
    case class Ok(resultCode: String) extends Result
    case class Err(errorCode: Int, errorMessage: String) extends Result
}
```
is mapped to
```scala mdoc
Json.schema[Result].stringify
```

## Value Classes
Value classes are mapped transparently to it's underlying type.
```scala
case class UserId(value: String) extends AnyVal
``` 
is mapped to
```scala
Json.schema[UserId].stringify
// res2: String = """{
//  "type": "string"
// }"""
```

## Dictionaries
`Scala` `Map` is mapped to `jsonschema` `object`. 

When it comes to KeyValue structures we can't skip Key topic. The problem is that
in scala `Map` may have any type for keys whereas in `json` object `properties` are always `strings`. 
So for `Map[String, T]` there are no problems, tut for the other types for key it may become problematic.
Happily `scala-jsonschema` has built-in support for this problem. 

### Key: String
So mapping of `Map[String, T]` is easy.
```scala mdoc
Json.schema[Map[String, String]].stringify
Json.schema[Map[String, Int]].stringify
Json.schema[Map[String, Color]].stringify
```   

### Key: Char
```scala mdoc
Json.schema[Map[Char, String]].stringify
```   

### Key: Numeric
```scala mdoc
Json.schema[Map[Byte, String]].stringify
Json.schema[Map[Short, String]].stringify
Json.schema[Map[Int, String]].stringify
Json.schema[Map[Long, String]].stringify
```  

### Key: Enum
Enums can also be used for keys, if your json-library provides corresponding support.

`scala-jsonschema` turns enum values into a regexp that is used in `patternProperties`.

```scala mdoc
Json.schema[Map[Color, String]].stringify
```   

### Key: Arbitrary
For the rest of types, if your json-library supports that, you can provide your typeclass
`KeyPattern[T]` that defines corresponding pattern for the key type.

```scala mdoc
import java.time.LocalTime
import json.Schema.dictionary._

implicit val localTimeKeyPattern: KeyPattern[LocalTime] = KeyPattern mk """^\d\d-\d\d$"""
Json.schema[Map[java.time.LocalTime, String]].stringify
``` 
 
## Arrays
Any scala type extended from `Iterable[T]` is mapped to `jsomschema` `array`.
Scala `Set[T]` is mapped to `array` with unique items.

```scala mdoc
Json.schema[List[String]].stringify
Json.schema[Set[Int]].stringify
``` 