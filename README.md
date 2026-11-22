# Scala JSON Schema
[![Support ukraine](https://img.shields.io/static/v1?label=United24&message=Support%20Ukraine&color=lightgrey&link=https%3A%2F%2Fu24.gov.ua&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAASwAAADICAYAAABS39xVAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAANKSURBVHhe7dZBThRhFEbRnx1IgvtFiIoxbgemOHLAhAoJ1QyaBahroKxqE%2BMS6iZncPKSbwE3b4yr6W58en4Z148zwC5tjbqabrdgvZ59PS5nn2eAfVobtbbquAXrcBquJ4B9Wht1%2BrQEC9g9wQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8gQLCBDsIAMwQIyBAvIECwgQ7CADMECMgQLyBAsIEOwgAzBAjIEC8j4F6zL%2BTA%2BHpfxYR0A9mhr1OXzPC5u7g%2Fvv%2F1YLr58B9ilU6Nu7ufx6%2BH88Hs6X9YLsEtbo34%2BvJvH29M4LC9jWZ4Admpt1NqqNVjTGqz5bFkmgJ1aG%2FX2KFhAgWABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVkCBaQIVhAhmABGYIFZAgWkCFYQIZgARmCBWQIFpAhWECGYAEZggVk%2FBes1%2BX4dwDYpbVRa6uOW7Du3p7Hy1YvgF3aGjWN2z9qCgwkg1n6XwAAAABJRU5ErkJggg%3D%3D)](https://u24.gov.ua)
[![Build](https://github.com/andyglow/scala-jsonschema/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/andyglow/scala-jsonschema/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/andyglow/scala-jsonschema/branch/master/graph/badge.svg?token=e8E62N8PiJ)](https://codecov.io/gh/andyglow/scala-jsonschema)
![](https://img.shields.io/static/v1.svg?label=Supported&nbsp;Scala&message=2.12,2.13,3.3&color=gray)
[![Version](https://img.shields.io/badge/dynamic/json.svg?label=Released&nbsp;Version&query=%24.response.docs%5B0%5D.latestVersion&url=https%3A%2F%2Fsearch.maven.org%2Fsolrsearch%2Fselect%3Fq%3Dg%3Aio.github.andyglow%2BAND%2Ba%3Ascala-jsonschema-core_2.13%26start%3D0%26rows%3D1)](https://search.maven.org/artifact/io.github.andyglow/scala-jsonschema-core_2.13/)

Generate [JSON Schema](https://json-schema.org) from Scala types at compile time, the way JSON libraries derive
their codecs. Inspired by Coursera's [AutoSchema](https://github.com/coursera/autoschema), but built on macros
instead of runtime reflection.

- Case classes (optional `Option` fields, default values), value classes, generic and recursive types
- Sealed families, Scala 3 `enum`s, `scala.Enumeration` and Java enums
- Collections and maps, `java.time`, `UUID`, `URI`, ... out of the box
- Titles and descriptions from annotations, from scaladoc, or passed in explicitly
- Validations (`pattern`, `minimum`, `maxItems`, ...)
- Renders Draft 04, 06, 07, 2019-09 and 2020-12, as its own JSON AST or as play-json, circe, spray-json, json4s or ujson
- Support for refined, cats, enumeratum and joda-time

Contents: [Install](#install) · [Quick start](#quick-start) · [Definitions and `$ref`s](#definitions-and-refs) ·
[Supported types](#supported-types) · [Sum types and enums](#sum-types-and-enums) ·
[Titles and descriptions](#titles-and-descriptions) · [Annotations](#annotations) · [Validation](#validation) ·
[Free objects](#free-objects) · [Rendering](#rendering-drafts-and-json-libraries) · [Modules](#modules) ·
[Migrating to 0.8.0](#migrating-to-080) · [Scala 3 notes](#scala-3-notes) · [Known issues](#known-issues)

## Install

```scala
libraryDependencies += "io.github.andyglow" %% "scala-jsonschema" % "0.8.0"
```

From 0.8.0 on, the groupId is **`io.github.andyglow`**. Releases up to 0.7.x are under `com.github.andyglow`.
The Scala packages (`json.*`, `com.github.andyglow.*`) haven't changed.

- Scala **2.12**, **2.13** and **3.3 LTS** (and later 3.x). Scala 2.11 support ended with 0.7.x.
- Runs on **Java 8** or later.
- `scala-jsonschema` pulls in `scala-jsonschema-core` and `scala-jsonschema-macros`.

Optional modules, all with the same version:

| artifact                       | what it adds                                     | built against                         |
|--------------------------------|--------------------------------------------------|---------------------------------------|
| `scala-jsonschema-play-json`   | `schema.asPlay(...)`, defaults via `Writes`      | `org.playframework:play-json` 3.0.x   |
| `scala-jsonschema-circe-json`  | `schema.asCirce(...)`, defaults via `Encoder`    | `io.circe:circe-core` 0.14.x          |
| `scala-jsonschema-spray-json`  | `schema.asSpray(...)`, defaults via `JsonWriter` | `io.spray:spray-json` 1.3.x           |
| `scala-jsonschema-json4s-json` | `schema.asJson4s(...)`                           | `org.json4s:json4s-core` 4.0.x        |
| `scala-jsonschema-ujson`       | `schema.asU(...)`                                | `com.lihaoyi:ujson`/`upickle` 4.x     |
| `scala-jsonschema-refined`     | refined types → validations                      | `eu.timepit:refined` 0.11.x           |
| `scala-jsonschema-cats`        | `NonEmptyList`, `NonEmptyMap`, ...               | `org.typelevel:cats-core` 2.x         |
| `scala-jsonschema-enumeratum`  | enumeratum enums                                 | `com.beachape:enumeratum` 1.9.x       |
| `scala-jsonschema-joda-time`   | joda-time types                                  | `joda-time:joda-time` 2.x             |
| `scala-jsonschema-derived`     | the `DerivedSchema[T]` type class                |                                       |
| `scala-jsonschema-parser`      | JSON and JSON Schema (draft-04 subset) → `json.Schema` | no dependencies                 |

Play JSON 3 is published as `org.playframework`. Projects still on `com.typesafe.play:play-json` 2.x should stay
on 0.7.x.

## Quick start

```scala
import json._
import json.schema.Version._

case class Company(name: String)
case class Person(name: String, nickname: Option[String], age: Int = 18, company: Company)

val schema: json.Schema[Person] = Json.schema[Person]

println(Json.stringify(schema, Draft07("https://example.com/person.json")))
```

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://example.com/person.json",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "name": { "type": "string" },
    "nickname": { "type": "string" },
    "age": { "default": 18, "type": "integer" },
    "company": {
      "type": "object",
      "additionalProperties": false,
      "properties": { "name": { "type": "string" } },
      "required": ["name"]
    }
  },
  "required": ["name", "company"]
}
```

`Option` fields and fields with a default value are optional. A default is written into the schema when it can be
turned into JSON: primitives, strings and collections/maps of them always can, other types need a `ToValue`, which
the JSON-library bridges derive from your encoders (`import com.github.andyglow.jsonschema.AsPlay._` etc.).

The same code works on Scala 3. There you can also derive the schema with the type:

```scala
case class Company(name: String) derives json.Schema
case class Person(name: String, nickname: Option[String], age: Int = 18, company: Company) derives json.Schema

val schema: json.Schema[Person] = Json.schema[Person] // or summon[json.Schema[Person]]
```

`derives json.Schema` declares a schema for the type, so each such type becomes a definition (next section):

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://example.com/person.json",
  "$ref": "#com.example.Person",
  "definitions": {
    "com.example.Company": { "$id": "#com.example.Company", "type": "object", ... },
    "com.example.Person": {
      "$id": "#com.example.Person",
      "type": "object",
      "properties": { ..., "company": { "$ref": "#com.example.Company" } },
      ...
    }
  }
}
```

The entry points (`json.Json`):

| call                                   | result                                                         |
|----------------------------------------|----------------------------------------------------------------|
| `Json.schema[T]`                       | `json.Schema[T]`, derived at compile time                      |
| `Json.objectSchema[T]("field" -> "description", ...)` | the schema of a case class as `json.Schema.object[T]`, with field descriptions |
| `Json.sig[T].signature`                | the type's name as used for definitions (`scala.List[com.example.Company]`) |
| `Json.stringify(schema, version)`      | the rendered JSON as a `String`                                |
| `import Json.auto._` (Scala 3: also `import Json.auto.given`) | an implicit `json.schema.Predef[T]` for every derivable `T` |

## Definitions and `$ref`s

For every type it meets, the macro looks for, in this order:

1. an implicit **`json.Schema[T]`**: the type becomes a **definition**, and every use of it is a `$ref` to it;
2. an implicit **`json.schema.Predef[T]`**: its schema is **inlined** where the type is used (all built-in types,
   `String`, numbers, `java.time`, collections, are `Predef`s);
3. neither: the schema is **derived** from the type's structure and inlined.

So you choose what becomes a definition by declaring an implicit `Schema` for it:

```scala
import json._

implicit val companySchema: json.Schema[Company] = Json.schema[Company]

val personSchema: json.Schema[Person] = Json.schema[Person]
```

```json
{
  "type": "object",
  "properties": {
    ...,
    "company": { "$ref": "#com.example.Company" }
  },
  "definitions": {
    "com.example.Company": { "$id": "#com.example.Company", "type": "object", ... }
  }
}
```

On Scala 3, `derives json.Schema` counts as such an implicit `Schema`. For a generic class
(`case class Box[A](value: A) derives json.Schema`), the derived `Schema[Box[X]]` exists only when `X` has an
implicit `Schema` itself (e.g. `X` derives too):

- `Box[Company]` (with `Company derives json.Schema`) is then an implicit `Schema` like any other: as a field it is a
  definition named `Box[Company]` (at the root, a summoned `Schema[Box[Company]]` isn't wrapped in a definition);
- `Box[Int]` has none (built-in types are `Predef`s), so it is derived inline where it is used.
  `summon[json.Schema[Box[Int]]]` doesn't compile; `Json.schema[Box[Int]]` always works.

A `Predef` provides a schema for a type without making it a definition, e.g. for a type the macro can't derive:

```scala
import json.Schema
import json.schema.Predef
import java.util.Currency

implicit val currencyPredef: Predef[Currency] = Predef(Schema.`string`[Currency])

case class Price(amount: BigDecimal, currency: Currency)

Json.schema[Price] // currency: {"type": "string"}, no definition
```

`import Json.auto._` gives a `Predef` for every type the macro can derive, for APIs that ask for one
(`def f[T](implicit p: Predef[T])`). These derived `Predef`s are inlined like any other, so the import doesn't
change what `Json.schema` derives: an explicit implicit `Schema` still becomes a definition.

```scala
import json.schema.Predef
import Json.auto._ // Scala 3: import Json.auto.given also works

def schemaOf[T](implicit p: Predef[T]): json.Schema[T] = p.schema

schemaOf[Person]
```

### Definition names

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
(`com.example.Box[scala.Int]` → `com.example.Box_scala.Int_`); see [Migrating to 0.8.0](#migrating-to-080).
`Json.sig` and the `json.Schema` value keep the readable name.

Recursive types (`case class Node(children: List[Node])`) become a definition automatically.

## Supported types

| Scala type                                                                  | JSON Schema                                     |
|-----------------------------------------------------------------------------|-------------------------------------------------|
| `String`, `Char`, `java.lang.Character`                                     | `string` (`Char`: exactly one character)        |
| `Boolean`                                                                   | `boolean`                                       |
| `Int`                                                                       | `integer`                                       |
| `Byte`, `Short`, `Long`, `BigInt`, `Float`, `Double`, `BigDecimal`          | `number`                                        |
| `java.time.Instant`, `LocalDateTime`, `ZonedDateTime`, `java.util.Date`, `java.sql.Timestamp` | `string`, format `date-time`  |
| `java.time.LocalDate`, `java.sql.Date`                                      | `string`, format `date`                         |
| `java.time.LocalTime`, `java.sql.Time`                                      | `string`, format `time`                         |
| `java.time.Duration`, `scala.concurrent.duration.Duration`                  | `string`, format `duration`                     |
| `java.util.UUID`                                                            | `string`, format `uuid`                         |
| `java.net.URI`, `java.net.URL`                                              | `string`, format `uri`                          |
| `Option[T]` field                                                           | `T`, not required                               |
| `Iterable[T]` (`List`, `Seq`, `Vector`, `Set`, `Array`, ...)                | `array` (`Set`: `uniqueItems`)                  |
| `Map[String, V]`, `Map[Int, V]`, ...                                        | `object` with `patternProperties`               |
| case class                                                                  | `object`                                        |
| value class                                                                 | its underlying type                             |
| sealed family, `enum`, `Enumeration`, Java enum                             | see [below](#sum-types-and-enums)               |

A `Map` key needs a known pattern: `String`, `Char`, `Byte`, `Short`, `Int`, `Long` and enums have one. For another
key type, provide one:

```scala
import json.Schema.dictionary.KeyPattern

implicit val uuidKey: KeyPattern[java.util.UUID] = KeyPattern.mk[java.util.UUID]("^[0-9a-f-]{36}$")
```

## Sum types and enums

```scala
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
```

- A sealed family of case objects is a string enum of their names: `Json.schema[Color]` →
  `{"type": "string", "enum": ["Red", "Green"]}`.
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
values are the codec's output. When their JSON type can't be known at compile time, add `@typeHint[T]`.

### Discriminators

`@discriminator` adds a field with the member's key to each member of a `oneOf`. The key is the member's full
name, or the `@discriminatorKey` you give it. With `phantom = false` the field must exist in each member.

```scala
import json.schema._

@discriminator(field = "kind") sealed trait Pet
object Pet {
  @discriminatorKey("cat") case class Cat(name: String) extends Pet
  @discriminatorKey("dog") case class Dog(name: String) extends Pet
}
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

### Enums as `oneOf` (`Flag.EnumsAsOneOf`)

A JSON Schema `enum` can't carry a title or description per value. With this implicit flag in scope, enums are
rendered as a `oneOf` of `const`s instead, which can:

```scala
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

## Titles and descriptions

There are three sources: annotations, scaladoc, and decorations passed to `Json.objectSchema`.

```scala
import json._
import json.schema._

@title("A Title")
@description("My perfect class")
case class Annotated(@description("A Param") a: String, @description("B Param") b: Int)

/** My perfect class
  *
  * @param a A Param
  * @param b B Param
  */
case class Documented(a: String, b: Int)

case class Plain(a: String, b: Int)

Json.objectSchema[Annotated]()
Json.objectSchema[Documented]() // the same descriptions, no title
Json.objectSchema[Plain]("a" -> "A Param", "b" -> "B Param").withTitle("A Title").withDescription("My perfect class")
```

All three give

```json
{
  "type": "object",
  "description": "My perfect class",
  "title": "A Title",
  "additionalProperties": false,
  "properties": {
    "a": { "type": "string", "description": "A Param" },
    "b": { "type": "integer", "description": "B Param" }
  },
  "required": ["a", "b"]
}
```

When several sources are present, the order is: decorations, then annotations, then scaladoc. Decorations suit
models defined in another module or library. `.withTitle`/`.withDescription` work on any schema.

**Scaladoc.** The doc comment right before a class (annotations, modifiers and ordinary comments may sit in between)
gives its description, and its `@param` tags the field descriptions. A class without a doc comment gets none.
A `@param` for a field that doesn't exist is a warning. Where the comment comes from:

- **Scala 2:** only classes compiled in the same compiler run as the `Json.schema` call. A model from another
  module or a jar, or a file an incremental build didn't recompile, has no descriptions.
- **Scala 3:** the same by default. Add `scalacOptions += "-Yread-docs"` to the module that calls `Json.schema`
  (or `derives`), and the scaladoc is also read from the TASTy of other modules, jars and earlier incremental runs.
  This needs the model's module to keep its docs in TASTy, which is the Scala 3 default.

## Annotations

Package `json.schema`:

| annotation                                    | on                    | effect                                                        |
|-----------------------------------------------|-----------------------|---------------------------------------------------------------|
| `@title("...")`                               | type, enum value      | `title`                                                       |
| `@description("...")`                         | type, field, enum value | `description`                                               |
| `@readOnly`                                   | field                 | `"readOnly": true`                                            |
| `@writeOnly`                                  | field                 | `"writeOnly": true`                                           |
| `@discriminator(field = "type", phantom = true)` | sealed root        | adds a discriminator field to each `oneOf` member             |
| `@discriminatorKey("key")`                    | sealed member         | the member's discriminator value (default: its full name)     |
| `@definition` / `@definition("name")`         | sealed member         | makes it a definition                                         |
| `@typeHint[T]`                                | sealed root, member   | the JSON type of enum values that come from a `ToValue`       |

On Scala 2, a field annotation can also target the underlying field: `@(description @field)("...")`.

## Validation

`withValidation` adds validation keywords. Each one is checked against the schema's type at compile time (a
`maxLength` on an `Int` doesn't compile).

```scala
import json._
import json.schema.validation.Instance._

Json.schema[String].withValidation(`minLength` := 1, `maxLength` := 100)
Json.schema[Int].withValidation(`minimum` := 0, `maximum` := 10)
Json.schema[Set[String]].withValidation(`maxItems` := 5)
```

| applies to         | keywords                                                                              |
|--------------------|---------------------------------------------------------------------------------------|
| numbers            | `multipleOf`, `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`            |
| strings            | `minLength`, `maxLength`, `pattern`, `contentEncoding`, `contentMediaType`, `contentSchema` |
| arrays             | `minItems`, `maxItems`, `uniqueItems`, `minContains`, `maxContains`                   |
| maps               | `minProperties`, `maxProperties`, `patternProperties`                                 |

For your own types (e.g. a value class), declare which group applies with a `json.schema.validation.Magnet`, as in
the [`UserId` example](#definition-names) above.

## Free objects

A free object allows any properties (`"additionalProperties": true`). Use it for a field that holds arbitrary JSON:

```scala
import play.api.libs.json.JsObject

case class Payload(id: String, metadata: JsObject)

implicit val metadataSchema: json.Schema[JsObject] = json.Schema.`object`.Free[JsObject]()
// or, to inline it instead of making it a definition:
// implicit val metadataPredef: json.schema.Predef[JsObject] = json.schema.Predef(json.Schema.`object`.Free[JsObject]())

val payloadSchema = json.Json.schema[Payload]
```

Any object schema can be made free, and back:

```scala
val personSchema       = Json.objectSchema[Person]()
val freePersonSchema   = personSchema.free   // additionalProperties: true
val strictPersonSchema = freePersonSchema.strict

strictPersonSchema == personSchema // true
```

## Rendering: drafts and JSON libraries

A `json.Schema` is independent of the JSON Schema draft and of any JSON library. You pick the draft when rendering:

| version (`json.schema.Version`) | `$schema`                                       | definitions             | `$ref`                        |
|---------------------------------|-------------------------------------------------|-------------------------|-------------------------------|
| `Draft04()`                     | `http://json-schema.org/draft-04/schema#`       | `definitions`           | `#/definitions/<name>`        |
| `Draft06(id)`, `Draft07(id)`    | `http://json-schema.org/draft-0{6,7}/schema#`   | `definitions` + `$id`   | `#<name>`                     |
| `Draft09(id)` (2019-09)         | `https://json-schema.org/draft/2019-09/schema`  | `$defs` + `$anchor`     | `#<name>`                     |
| `Draft12(id)` (2020-12)         | `https://json-schema.org/draft/2020-12/schema`  | `$defs` + `$anchor`     | `#<name>`                     |

```scala
import json._
import json.schema.Version._
import com.github.andyglow.jsonschema.AsValue

Json.stringify(schema, Draft12("https://example.com/person.json")) // String
AsValue.schema(schema, Draft04())                                    // com.github.andyglow.json.Value, the library's own AST
```

With a bridge module, render into your JSON library's AST:

```scala
import json.schema.Version._

import com.github.andyglow.jsonschema.AsPlay._    // json.Json.schema[Foo].asPlay(Draft07("id"))    : play.api.libs.json.JsObject
import com.github.andyglow.jsonschema.AsCirce._   // json.Json.schema[Foo].asCirce(Draft07("id"))   : io.circe.Json
import com.github.andyglow.jsonschema.AsSpray._   // json.Json.schema[Foo].asSpray(Draft07("id"))   : spray.json.JsValue
import com.github.andyglow.jsonschema.AsJson4s._  // json.Json.schema[Foo].asJson4s(Draft07("id")) : org.json4s.JValue
import com.github.andyglow.jsonschema.AsU._       // json.Json.schema[Foo].asU(Draft07("id"))       : ujson.Value
```

These libraries have their own `Json` object, so write `json.Json.schema` when you import their package with `_`.
The bridges also provide `ToValue` from the library's encoders, so default values of your own types get rendered.

## Modules

### Refined

```scala
import com.github.andyglow.jsonschema.RefinedSupport._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._

case class Signup(
  login: String Refined MatchesRegex["^[a-z][a-z0-9]{2,15}$"], // {"type": "string", "pattern": "^[a-z][a-z0-9]{2,15}$"}
  age: Int Refined Greater[17],                                // {"type": "integer", "exclusiveMinimum": 17}
  tags: List[String] Refined MaxSize[5]                        // {"type": "array", "maxItems": 5, ...}
)

Json.schema[Signup]
```

Predicates are written with literal types on Scala 2.13 and 3. On Scala 2.12 use shapeless's witnesses:
``Greater[W.`17`.T]``. Supported predicates:

- boolean: `And`, `Or`, `Not`
- numeric: `Positive`, `Negative`, `NonPositive`, `NonNegative`, `Greater`, `Less`, `GreaterEqual`, `LessEqual`, `Divisible`
- string: `MatchesRegex`, `StartsWith`, `EndsWith`, `Uuid`, `Uri`, `Url`, `IPv4`, `IPv6`, `Xml`, `Trimmed`
- collection (strings and collections): `Size`, `MinSize`, `MaxSize`, `Empty`, `NonEmpty`

### Cats

```scala
import com.github.andyglow.jsonschema.CatsSupport._
import cats.data.{NonEmptyList, NonEmptyMap}

case class Team(members: NonEmptyList[String], scores: NonEmptyMap[String, Int])

Json.schema[Team] // members: array with "minItems": 1; scores: object with "minProperties": 1
```

Supported: `NonEmptyList`, `NonEmptyVector`, `NonEmptySet`, `NonEmptyChain`, `NonEmptyMap`, `OneAnd`,
`NonEmptyLazyList` (2.13, 3) / `NonEmptyStream` (2.12), `Chain`.

### Enumeratum

```scala
import com.github.andyglow.jsonschema.EnumeratumSupport._
```

`Enum` entries give a string enum of their names. `IntEnum`, `LongEnum`, `ShortEnum`, `ByteEnum` (and the
string/char value enums) give an enum of their values. They work as `Map` keys too.

### Joda-Time

```scala
import com.github.andyglow.jsonschema.JodaTimeSupport._
```

Adds `org.joda.time.Instant`, `DateTime`, `LocalDateTime` (format `date-time`), `LocalDate` (`date`), `LocalTime`
(`time`) and `Duration` (`duration`). They are implicit `Schema`s, so they appear as definitions. To inline them
instead, import the `Predef`s: `import com.github.andyglow.jsonschema.JodaTimeSupport.predef._`.

### DerivedSchema

`scala-jsonschema-derived` provides `json.schema.derived.DerivedSchema[T]`, a type class for "a schema derived
from `T`'s structure", e.g. for a context bound:

```scala
import json.schema.derived.DerivedSchema

def describe[T](implicit ds: DerivedSchema[T]): json.Schema[T] = ds.schema
```

The root type is always derived and inlined, even when an implicit `Schema[T]` exists (nested types follow the
usual rules). A type that can't be derived has no instance, so a default argument or a lower-priority implicit
applies instead.

### Parser

`scala-jsonschema-parser` goes the other way: `com.github.andyglow.json.ParseJson(string)` parses JSON into the
library's AST, and `com.github.andyglow.jsonschema.ParseJsonSchema(string)` parses a JSON Schema (a draft-04
subset) into a `Try[json.Schema[_]]`.

## Migrating to 0.8.0

The full list is in the [CHANGELOG](CHANGELOG.md).

**Build**

- The groupId is `io.github.andyglow` (was `com.github.andyglow`). Imports don't change.
- Scala 2.11 is no longer published. Scala 3.3 is.
- Bridges and modules are built against current library versions, notably play-json 3 (`org.playframework`).

**Output that changes**

- **Definition names in the rendered JSON follow each draft's grammar.** Characters a draft doesn't allow are
  replaced with `_` in `definitions`/`$defs` keys, `$id`, `$anchor` and `$ref`: `com.acme.Box[scala.Int]` →
  `com.acme.Box_scala.Int_`. Draft-04 keeps the characters valid in a URI fragment; drafts 06, 07 and 2019-09
  allow `[A-Za-z][-A-Za-z0-9.:_]*`; 2020-12 allows `[A-Za-z_][-A-Za-z0-9._]*` (a name that has to start with a
  letter gets a prefix). If two names collide, the one that already conformed keeps it and the others get `_2`,
  `_3`, ... Names that already conform (`com.acme.Foo`) don't change. `Json.sig`, the `json.Schema` value and
  the raw renderer keep the readable name.
- **A type alias is seen through.** A definition reached through an alias is named after the type it stands for,
  so a root implicit `Schema[String]` is `java.lang.String` (was `scala.Predef.String`), like a `String` field.
  Type arguments keep their spelling: `Box[String]` is still `Box[scala.Predef.String]`.
- **An implicit `Schema` is used only for exactly its type.** An implicit `Schema[Sub]` no longer stands in for its
  sealed parent (that dropped the other subtypes), nor makes a non-sealed trait derivable.
- **Scala 2: `Json.sig` of a backticked type name is decoded** (`` pkg.`odd name` `` → `pkg.odd name`, was
  `pkg.odd$u0020name`), as on Scala 3. The renderers then make it a valid name (`odd_name`).
- **Scala 2: an undocumented class gets no description.** It used to take the nearest earlier doc comment in its
  file (of another class, a method, ...), including its `@param`s. Only the doc comment right before the
  declaration counts now.
- **Scala 2: field annotations with the `@field` meta-annotation are read** (`@(description @field)("...")`);
  they used to be ignored.
- Copies of a free object stay free (`withTitle`, `withDescription`, `withField`, `toDefinition`, ... used to make
  it strict).

**API**

- `json.Schema` equality is structural: a `ref` compares its signature (any two refs used to be equal), a field
  compares its description, a free object doesn't equal the strict one.
- `def.toDefinition(name)` renames only its own definition and the refs to it.
- `import Json.auto._` works (it used to crash the compiler). See [above](#definitions-and-refs) for what it does.
- A scaladoc `@param` for a field that doesn't exist is a warning (it used to fail on the root type).
- Many cases that crashed the macro or produced uncompilable code are now clear compile errors (a `Map` key
  without a pattern, a `mutable.Map`, structural types, ...), and some now just work: local case classes, Java
  enums, generic value classes, `Map` subtypes such as `SortedMap` and `ListMap` with a key pattern.
- refined: the implicit is `refinedJsonSchema[R <: Refined[A, B], A, B]` (was `[A, B]`). This matters only if you
  pass its type arguments explicitly. On Scala 3, predicates take literal types (`Greater[5]`) instead of
  ``W.`5`.T``.
- parser: `ParseJson` is a strict RFC 8259 parser (it used to accept invalid JSON and didn't decode escapes).
  Failures are `JsonParseException`s with the offset; `MutableValue` and the Java classes are gone.

## Scala 3 notes

What's new on Scala 3: `derives json.Schema`, native `enum`s, `import Json.auto.given`, and `-Yread-docs` for
scaladoc from other modules (see [Titles and descriptions](#titles-and-descriptions)). Everything else is shared
with Scala 2 and gives the same schemas. The deliberate differences:

- **Aliases that only rename a type are gone before the macro sees them.** For `type MyList[A] = List[A]`,
  `Json.sig` and definition names show `scala.List[scala.Int]` (Scala 2: `pkg.MyList[scala.Int]`). Standard
  collections in type arguments are spelled the usual way (`scala.List`, `scala.Predef.Map`) even if you wrote
  `scala.collection.immutable.List`, so a name doesn't depend on where the model was compiled. Other aliases
  (`type Users = List[User]`) give the same names as on Scala 2.
- **Sealed families can't have anonymous or block-local subclasses** (`new Root {}`, a case class declared inside
  a method): a compile error, because Scala 3 can't list them. A local case class on its own works, also with
  default values (Scala 2 rejects those defaults).
- **`Enumeration`:** the enum lists only the object's `Value`s; a class declared inside an `Enumeration` object
  is derived as a class.
- **`Option` defaults.** On both versions `= None` gives no default, and any other `Option` default needs a
  `ToValue[Option[X]]` (the bridges provide one). Scala 2 evaluates the default at compile time to see whether it
  is `None`; Scala 3 recognizes only `None` and `Option.empty` written out, so `= Defaults.none`, `= (None)` or
  `= { None }` need the `ToValue` too.
- **A generic `derives` class** has a `Schema[Box[X]]` only when `X` has an implicit `Schema`: then `Box[X]` is a
  definition, otherwise it is derived inline (see [Definitions](#definitions-and-refs)). Inside the derived
  schema, an implicit `Schema` for an abstract type parameter is used as it is at runtime: an existing definition
  keeps its name, anything else is inlined.
- **refined:** `refined[T]` has no `T <: Refined[_, _]` bound (Refined is an opaque type on Scala 3; a non-refined
  `T` is a compile error), and an unsupported predicate found while deriving fails with the derivation's
  `is not supported` error followed by refined's own message.
- Compile errors quote Scala 3 code, so their text differs from Scala 2's.

To see the code the macro generates, compile with `-Djsonschema.print-code=true` (e.g.
`sbt -Djsonschema.print-code=true compile`): every expansion is printed as an info message.

Not supported (yet): union types, literal types, opaque types (beyond refined) and named tuples; Scala.js.

## Known issues

- **Scala 3: recursive generic `derives`.** For `case class Tree[A](value: A, children: List[Tree[A]]) derives json.Schema`,
  `Tree[Int]` used as a field or as the root of `Json.schema` is a definition named `pkg.Tree[scala.Int]`, but a
  summoned `Schema[Tree[X]]` (e.g. `summon[json.Schema[Tree[Company]]]`, `Company derives json.Schema`) and a
  `Tree[A]` inside another generic `derives` class keep the type parameter's name (`pkg.Tree[pkg.Tree.A]`), so two
  instantiations in one schema share it.
- **Scala 2: an implicit declared on the line just before a `Json.schema` call may be ignored.** The Scala 2 macro
  recognizes `implicit val x: Schema[X] = Json.schema[X]` (so that it doesn't refer to itself) by reading the
  source text around the call, and a name on the previous line can match. Keep a blank line between such
  implicits.
- Tuples are objects (`{"_1": ..., "_2": ...}`) and `Either[L, R]` is a `oneOf` of `{"value": L}` and `{"value": R}`.
- On draft-04, `exclusiveMinimum`/`exclusiveMaximum` are rendered as numbers (draft-06 style).

## Documentation site

The site sources are in `docs/src/main/paradox`. Build it with `sbt makeDocs` (output in `docs/target/site`).
