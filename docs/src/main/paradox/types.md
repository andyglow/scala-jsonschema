# Types and Enums

## Supported types

| Scala type                                                                  | JSON Schema                                     |
|-----------------------------------------------------------------------------|-------------------------------------------------|
| `String`, `Char`, `java.lang.Character`                                     | `string` (`Char`: exactly one character)        |
| `Boolean`, `java.lang.Boolean`                                              | `boolean`                                       |
| `Int`, `java.lang.Integer`                                                  | `integer`                                       |
| `Byte`, `Short`, `Long`, `BigInt`, `Float`, `Double`, `BigDecimal`          | `number`                                        |
| `java.lang.Byte`/`Short`/`Long`/`Float`/`Double`, `java.math.BigInteger`/`BigDecimal` | `number` (as the Scala type)          |
| `java.time.Instant`, `LocalDateTime`, `ZonedDateTime`, `java.util.Date`, `java.sql.Timestamp` | `string`, format `date-time`  |
| `java.time.LocalDate`, `java.sql.Date`                                      | `string`, format `date`                         |
| `java.time.LocalTime`, `java.sql.Time`                                      | `string`, format `time`                         |
| `java.time.Duration`, `scala.concurrent.duration.Duration`                  | `string`, format `duration`                     |
| `java.util.UUID`                                                            | `string`, format `uuid`                         |
| `java.net.URI`, `java.net.URL`                                              | `string`, format `uri`                          |
| `Option[T]`                                                                 | `T` or `null`; a field is not required (@ref:[more](rendering.md#option)) |
| `Iterable[T]` (`List`, `Seq`, `Vector`, `Set`, `Array`, ...)                | `array` (`Set`: `uniqueItems`)                  |
| `Map[String, V]`, `Map[Int, V]`, ...                                        | `object` with `patternProperties`               |
| case class                                                                  | `object`                                        |
| value class                                                                 | its underlying type                             |

A `Map` key needs a known pattern: `String`, `Char`, `Byte`, `Short`, `Int`, `Long` (and their `java.lang` boxes) and enums have one. For another
key type, provide one:

```scala mdoc:compile-only
import json.Schema.dictionary.KeyPattern

implicit val uuidKey: KeyPattern[java.util.UUID] = KeyPattern.mk[java.util.UUID]("^[0-9a-f-]{36}$")

json.Json.schema[Map[java.util.UUID, String]]
```

## Sum types and enums

```scala mdoc:compile-only
import json._

sealed trait Color
object Color {
  case object Red extends Color
  case object Green extends Color
}

sealed trait Shape
object Shape {
  case class Circle(radius: Double) extends Shape
  case class Rectangle(width: Double, height: Double) extends Shape
}

Json.schema[Color] // {"type": "string", "enum": ["Red", "Green"]}
Json.schema[Shape] // {"oneOf": [{"properties": {"radius": ...}, ...}, {"properties": {"width": ..., "height": ...}, ...}]}
```

- A sealed family of case objects is a string enum of their names.
- A sealed family with case classes is a `oneOf` of its members' schemas (case objects among them are merged into
  one enum member).
- `scala.Enumeration` values and Java enums (`java.time.DayOfWeek`, ...) are string enums of their names.
- Scala 3 `enum`s follow the same rules: only singleton cases give a string enum, cases with parameters a `oneOf`.

```scala
// Scala 3
enum Status { case Active, Suspended }                 // {"type": "string", "enum": ["Active", "Suspended"]}

enum Event {                                           // {"oneOf": [{"additionalProperties": false, "properties": {"id": ...}, ...}, ...]}
  case Created(id: String)
  case Deleted(id: String, reason: Option[String])
}
```

If an implicit `ToValue` for the enum type is in scope (e.g. from a JSON-library bridge and your codec), the enum
values are the codec's output. Their schema is your `@typeHint[T]` if you give one; otherwise it comes from the
values themselves when the schema is rendered: strings make a string enum, any other value is a `const` of it.

These are the default shapes. The derived `family` lists the members in declaration order; a rendered `oneOf` or
`enum` keeps that order up to 4 entries (with more, the order is fixed, but it is hash order). To match how a JSON library writes sealed families
(play's `_type`, circe's wrapper object, ...), render with that library's profile: see
@ref:[Library profiles](rendering.md#library-profiles).

## Discriminators

`@discriminator` adds a field with the member's key to each member of a `oneOf`. The key is the member's full
name, or the `@discriminatorKey` you give it. With `phantom = false` the field must exist in each member.

```scala mdoc:compile-only
import json._
import json.schema._

@discriminator(field = "kind") sealed trait Pet
object Pet {
  @discriminatorKey("cat") case class Cat(name: String) extends Pet
  @discriminatorKey("dog") case class Dog(name: String) extends Pet
}

Json.schema[Pet]
```

```json
{
  "oneOf": [
    {
      "additionalProperties": false,
      "properties": { "name": { "type": "string" }, "kind": { "enum": ["cat"] } },
      "required": ["name", "kind"]
    },
    {
      "additionalProperties": false,
      "properties": { "name": { "type": "string" }, "kind": { "enum": ["dog"] } },
      "required": ["name", "kind"]
    }
  ]
}
```

`@definition` (or `@definition("name")`) on a member makes that member a definition.

## Enums as oneOf

A JSON Schema `enum` can't carry a title or description per value. With the implicit flag
`Flag.EnumsAsOneOf` in scope, enums are rendered as a `oneOf` of `const`s instead, which can:

```scala mdoc:compile-only
import json._
import json.schema._

sealed trait Gender
object Gender {
  @title("The Male") case object Male extends Gender

  /** The Female */
  case object Female extends Gender
}

implicit val flags: Flag with Flag.EnumsAsOneOf = null

Json.schema[Gender]
```

```json
{
  "oneOf": [
    { "title": "The Male", "const": "Male" },
    { "description": "The Female", "const": "Female" }
  ]
}
```
