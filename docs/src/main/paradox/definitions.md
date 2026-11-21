# Definitions

For every type it meets, the macro looks for, in this order:

1. an implicit **`json.Schema[T]`**: the type becomes a **definition**, and every use of it is a `$ref` to it;
2. an implicit **`json.schema.Predef[T]`**: its schema is **inlined** where the type is used (all built-in types,
   `String`, numbers, `java.time`, collections, are `Predef`s);
3. neither: the schema is **derived** from the type's structure and inlined.

So you choose what becomes a definition by declaring an implicit `Schema` for it:

```scala mdoc:compile-only
import json._

case class Company(name: String)
case class Person(name: String, company: Company)

implicit val companySchema: json.Schema[Company] = Json.schema[Company]

val personSchema: json.Schema[Person] = Json.schema[Person] // company: {"$ref": ...}, plus a definition
```

On Scala 3, `derives json.Schema` counts as such an implicit `Schema`. The schema `derives` makes for a generic
class (`case class Box[A](value: A) derives json.Schema`) is inlined instead: each `Box[Int]`, `Box[String]` is
derived where it is used. `summon[json.Schema[Box[Int]]]` needs an implicit `Schema` for `Int`, which built-in
types don't have; `Json.schema[Box[Int]]` always works.

Recursive types (`case class Node(children: List[Node])`) become a definition automatically.

## Predefs

A `Predef` provides a schema for a type without making it a definition, e.g. for a type the macro can't derive:

```scala mdoc:compile-only
import json._
import json.schema.Predef
import java.util.Currency

implicit val currencyPredef: Predef[Currency] = Predef(Schema.`string`[Currency])

case class Price(amount: BigDecimal, currency: Currency)

Json.schema[Price] // currency: {"type": "string"}, no definition
```

`import Json.auto._` gives a `Predef` for every type the macro can derive, for APIs that ask for one. These
derived `Predef`s are inlined like any other, so the import doesn't change what `Json.schema` derives: an
explicit implicit `Schema` still becomes a definition.

```scala mdoc:compile-only
import json._
import json.schema.Predef
import Json.auto._ // Scala 3: import Json.auto.given also works

def schemaOf[T](implicit p: Predef[T]): json.Schema[T] = p.schema

case class Foo(a: Int)

schemaOf[Foo]
```

## Definition names

A definition is named after the type's full name, with its type arguments: `com.example.Company`,
`com.example.Box[scala.Int]`. Rename one with `toDefinition`. This is also the way to have two schemas of the same
type, e.g. a validated one:

```scala
import json._
import json.schema.validation.Instance._
import json.schema.validation.Magnet

case class UserId(value: String) extends AnyVal
case class User(id: UserId, name: String)

// a value class has no built-in validation bound: say which validations apply to it
implicit val userIdMagnet: Magnet[UserId, String] = Magnet.mk[UserId, String]

implicit val userIdSchema: json.Schema[UserId] =
  Json.schema[UserId].toDefinition("user-id").withValidation(`pattern` := "^[a-f0-9]{16}$")

val userSchema: json.Schema[User] = Json.schema[User]
```

```json
{
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "id": { "$ref": "#user-id" },
    "name": { "type": "string" }
  },
  "required": ["id", "name"],
  "definitions": {
    "user-id": { "$id": "#user-id", "type": "string", "pattern": "^[a-f0-9]{16}$" }
  }
}
```

In the rendered JSON, characters a draft doesn't allow in a definition name are replaced with `_`
(`com.example.Box[scala.Int]` becomes `com.example.Box_scala.Int_`); see @ref:[Migrating to 0.8.0](migration.md).
`Json.sig` and the `json.Schema` value keep the readable name.
