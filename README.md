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
[Free objects](#free-objects) · [Any JSON value](#any-json-value) · [Rendering](#rendering-drafts-and-json-libraries) · [Modules](#modules) ·
[Migrating to 0.9.0](#migrating-to-090) · [Migrating to 0.8.0](#migrating-to-080) · [Scala 3 notes](#scala-3-notes) · [Known issues](#known-issues)

## Install

```scala
libraryDependencies += "io.github.andyglow" %% "scala-jsonschema" % "0.9.0"
```

From 0.8.0 on, the groupId is **`io.github.andyglow`**. Releases up to 0.7.x are under `com.github.andyglow`.
The Scala packages (`json.*`, `com.github.andyglow.*`) haven't changed.

- Scala **2.12**, **2.13** and **3.3 LTS** (and later 3.x). Scala 2.11 support ended with 0.7.x.
- Runs on **Java 8** or later.
- `scala-jsonschema` pulls in `scala-jsonschema-core` and `scala-jsonschema-macros`.

Optional modules, all with the same version:

| artifact                       | what it adds                                     | built against                         | Scala.js |
|--------------------------------|--------------------------------------------------|---------------------------------------|----------|
| `scala-jsonschema-play-json`   | `schema.asPlay(...)`, defaults via `Writes`      | `org.playframework:play-json` 3.0.x   | ✓        |
| `scala-jsonschema-circe-json`  | `schema.asCirce(...)`, defaults via `Encoder`    | `io.circe:circe-core` 0.14.x          | ✓        |
| `scala-jsonschema-spray-json`  | `schema.asSpray(...)`, defaults via `JsonWriter` | `io.spray:spray-json` 1.3.x           | JVM only |
| `scala-jsonschema-json4s-json` | `schema.asJson4s(...)`                           | `org.json4s:json4s-core` 4.0.x        | JVM only |
| `scala-jsonschema-ujson`       | `schema.asU(...)`                                | `com.lihaoyi:ujson`/`upickle` 4.x     | ✓        |
| `scala-jsonschema-refined`     | refined types → validations                      | `eu.timepit:refined` 0.11.x           | ✓        |
| `scala-jsonschema-cats`        | `NonEmptyList`, `NonEmptyMap`, ...               | `org.typelevel:cats-core` 2.x         | ✓        |
| `scala-jsonschema-enumeratum`  | enumeratum enums                                 | `com.beachape:enumeratum` 1.9.x       | ✓        |
| `scala-jsonschema-joda-time`   | joda-time types                                  | `joda-time:joda-time` 2.x             | JVM only |
| `scala-jsonschema-derived`     | the `DerivedSchema[T]` type class                |                                       | ✓        |
| `scala-jsonschema-parser`      | JSON and JSON Schema (draft-04 subset) → `json.Schema` | no dependencies                 | ✓        |

Play JSON 3 is published as `org.playframework`. Projects still on `com.typesafe.play:play-json` 2.x should stay
on 0.7.x.

### Scala.js

From 0.9.0 on, `scala-jsonschema` (with `-core` and `-macros`) and the modules marked ✓ above are also published
for Scala.js 1.x, for Scala 2.12, 2.13 and 3. Use `%%%`:

```scala
libraryDependencies += "io.github.andyglow" %%% "scala-jsonschema" % "0.9.0"
```

- Needs **Scala.js 1.23.0** or later (`sbt-scalajs` 1.23.0): a project's linker must be at least as new as the
  Scala.js IR in the jars it links.
- spray-json, json4s and joda-time have no Scala.js builds, so their modules are JVM only.
- Schemas are derived at compile time, so everything works as on the JVM: scaladoc descriptions, `derives json.Schema`,
  refined types, defaults. On Scala 3, scaladoc of classes from another module needs `-Yread-docs`, as on the JVM.
- `java.time` types (`LocalDate`, `Instant`, ...) need no extra dependency: their schema only names the type. Values of
  those types at run time need a `java.time` implementation such as
  [scala-java-time](https://github.com/cquiroz/scala-java-time), as in any Scala.js code. So does the schema of a
  Java enum from `java.time` (`DayOfWeek`, `Month`): a Java enum's schema lists its values at run time.
  `java.util.UUID` and `java.net.URI` are part of Scala.js; `java.sql` types don't exist there.
  The schemas (and models with fields) of `Locale`, `Currency`, `InetAddress`, `Path` and `File` compile and link on
  Scala.js; creating values of `Locale`/`Currency` needs [scala-java-locales](https://github.com/cquiroz/scala-java-locales),
  the others have no Scala.js implementation.
- Numbers render as on the JVM: a `Double` bound of `16.0` is `16.0` in the JSON, not `16`.

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
    "nickname": { "type": ["string", "null"] },
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

`Option` fields and fields with a default value are optional. An `Option[X]` is "X or null" wherever it appears
(see [`Option`](#option)). A default is written into the schema when it can be turned into JSON: primitives, strings
and collections/maps of them always can, other types need a `ToValue`, which the JSON-library bridges derive from
your encoders (`import com.github.andyglow.jsonschema.AsPlay._` etc.).

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

One implicit for every instantiation of a generic class works too:

```scala
import json._

case class Company(name: String)
case class Team(name: String)
case class Box[A](value: A)
case class Org(company: Box[Company], team: Box[Team])

implicit def boxSchema[A: json.Schema]: json.Schema[Box[A]] = Json.schema[Box[A]]
implicit val companySchema: json.Schema[Company]             = Json.schema[Company]
implicit val teamSchema: json.Schema[Team]                   = Json.schema[Team]

val orgSchema: json.Schema[Org] = Json.schema[Org]
```

`Box[Company]` and `Box[Team]` are two definitions, named after the concrete types. Inside `boxSchema`, though, `A`
is a type parameter, so no name for it is known at compile time: the `json.Schema[A]` it receives is used as it is.
So **`Company` is inlined inside `Box[Company]`**, although it has an implicit `Schema` (a plain
`case class X(c: Company)` gives `"$ref": "#com.example.Company"`). To keep it a definition there, give the instance
a name: `Json.schema[Company].toDefinition("com.example.Company")` (on Scala 3, `derives json.Schema` does that).

```json
{
  "type": "object",
  "properties": {
    "company": { "$ref": "#com.example.Box_com.example.Company_" },
    "team": { "$ref": "#com.example.Box_com.example.Team_" }
  },
  "definitions": {
    "com.example.Box_com.example.Company_": {
      "$id": "#com.example.Box_com.example.Company_",
      "type": "object",
      "properties": { "value": { "type": "object", "properties": { "name": { "type": "string" } }, ... } },
      ...
    },
    "com.example.Box_com.example.Team_": { "$id": "#com.example.Box_com.example.Team_", ... }
  }
}
```

A recursive generic class has a known limitation, see [Known issues](#known-issues).

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
| `Boolean`, `java.lang.Boolean`                                              | `boolean`                                       |
| `Int`, `java.lang.Integer`                                                  | `integer`                                       |
| `Byte`, `Short`, `Long`, `BigInt`, `Float`, `Double`, `BigDecimal`          | `number`                                        |
| `java.lang.Byte`/`Short`/`Long`/`Float`/`Double`, `java.math.BigInteger`/`BigDecimal` | `number` (as the Scala type)          |
| `java.time.Instant`, `LocalDateTime`, `ZonedDateTime`, `OffsetDateTime`, `java.util.Date`, `java.sql.Timestamp` | `string`, format `date-time` |
| `java.time.LocalDate`, `java.sql.Date`                                      | `string`, format `date`                         |
| `java.time.LocalTime`, `OffsetTime`, `java.sql.Time`                        | `string`, format `time`                         |
| `java.time.Duration`, `scala.concurrent.duration.Duration`, `FiniteDuration` | `string`, format `duration`                    |
| `java.time.Period`, `Year`, `YearMonth`, `MonthDay`, `ZoneOffset`           | `string` with a pattern of the text java.time writes (`P1Y-2M3D`, `2024`, `2024-03`, `--12-03`, `+01:00`) |
| `java.time.ZoneId`                                                          | `string`                                        |
| `java.util.UUID`                                                            | `string`, format `uuid`                         |
| `java.net.URI`, `java.net.URL`                                              | `string`, format `uri`                          |
| `java.util.regex.Pattern`, `scala.util.matching.Regex`                      | `string`, format `regex`                        |
| `java.net.Inet4Address`, `java.net.Inet6Address`                            | `string`, format `ipv4` / `ipv6`                |
| `java.util.Currency`                                                        | `string`, pattern of an ISO 4217 code (`USD`)   |
| `java.nio.charset.Charset`                                                  | `string`, pattern of a charset name (`UTF-8`)   |
| `java.util.Locale`, `java.net.InetAddress`, `java.nio.file.Path`, `java.io.File` | `string`                                   |
| `Option[T]`                                                                 | `T` or `null`; a field is not required ([more](#option)) |
| `java.util.Optional[T]`                                                     | the same as `Option[T]`, under every profile    |
| `Iterable[T]` (`List`, `Seq`, `Vector`, `Set`, `Array`, ...)                | `array` (`Set`: `uniqueItems`)                  |
| `java.util.Collection[T]` (`List`, `Set`, `ArrayList`, `EnumSet`, ...)       | `array` (`java.util.Set`: `uniqueItems`)        |
| `Map[String, V]`, `Map[Int, V]`, ...                                        | `object` with `patternProperties` (keys enforced) |
| `java.util.Map[K, V]` (`HashMap`, `TreeMap`, `EnumMap`, ...)                | the same as `Map[K, V]`                         |
| case class, tuple                                                           | `object`, `properties` in declaration order     |
| value class                                                                 | its underlying type                             |
| sealed family, `enum`, `Enumeration`, Java enum                             | see [below](#sum-types-and-enums)               |

A `Map` key needs a known pattern (a `KeyPattern`): `String`, `Char`, `Byte`, `Short`, `Int`, `Long` (negative keys
included; also their `java.lang` boxes), `java.util.UUID`, `Boolean`, value classes of these and enums have one. For
another key type, provide one (one in scope also replaces a built-in one):

```scala
import json.Schema.dictionary.KeyPattern

implicit val dateKey: KeyPattern[java.time.LocalDate] = KeyPattern.mk[java.time.LocalDate]("^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
```

The pattern is the `patternProperties` key, and it is enforced: a key that doesn't match it is rejected
(`propertyNames` from draft 06 on, `additionalProperties: false` on draft 04). `Map[String, V]` accepts any key.
play-json writes a `Map[UUID, V]` as an array of pairs (it has no `KeyWrites[UUID]`), which this object schema doesn't
describe: give play a `KeyWrites`/`KeyReads` for the key, or give the map type a `Predef`.

`Array[Byte]` is an `array` of numbers, which is how circe, play-json, upickle, spray-json and json4s write it. Jackson
writes a base64 string instead; for that, use `Predef.bytesAsBase64` (`{"type": "string", "contentEncoding":
"base64", "pattern": ...}`):

```scala
implicit val bytes: json.schema.Predef[Array[Byte]] = json.schema.Predef.bytesAsBase64
```

### A `null` schema

`json.Schema.`null`` is `{"type": "null"}`. A field of type `Null` needs it as a `Predef`:
`implicit val n: json.schema.Predef[Null] = json.schema.Predef(json.Schema.`null`)`. For a JSON library's own null
(e.g. a field that is always `null`), the bridges have one, opt-in: `import com.github.andyglow.jsonschema.AsPlay.predef._`
(`JsNull`), `AsSpray.predef._` (`JsNull`), `AsJson4s.predef._` (`JNull`), `AsU.predef._` (`ujson.Null`).

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
- When one document could match two members (the same fields, or one member's fields plus optional ones), the
  family is an `anyOf` instead, because a `oneOf` would reject that document. Members with identical schemas appear
  once. A family whose members are told apart by a tag (a discriminator, a wrapper object) is always a `oneOf`.
- `scala.Enumeration` values and Java enums (`java.time.DayOfWeek`, ...) are string enums of their names.
- Scala 3 `enum`s follow the same rules: only singleton cases give a string enum, cases with parameters a `oneOf`.

```scala
// Scala 3
enum Status { case Active, Suspended }                 // {"type": "string", "enum": ["Active", "Suspended"]}

enum Event {                                           // {"type": "object", "anyOf": [{"additionalProperties": false, "properties": {"id": ...}, ...}, ...]}
  case Created(id: String)                             // {"id": "x"} is a Created and a Deleted: anyOf
  case Deleted(id: String, reason: Option[String])
}
```

If an implicit `ToValue` for the enum type is in scope (e.g. from a JSON-library bridge and your codec), the enum
values are the codec's output. Their schema is your `@typeHint[T]` if you give one; otherwise it comes from the
values themselves when the schema is rendered: strings make a string enum, any other value is a `const` of it.

These are the default shapes. The derived `family` lists the members in declaration order (on Scala 2, a family
compiled in another module is in name order: its class files have no source positions). A rendered `oneOf` or
`anyOf` keeps that order, including the `oneOf` of `const`s that `Flag.EnumsAsOneOf` makes (so do
`Schema.oneof.of(...)`, `allof.of(...)` and `anyof.of(...)`; a `oneof(Set(...))` you build yourself has its `Set`'s
order). A rendered `enum` keeps it whatever its size. An `enum` you build yourself keeps your order when you use
`Schema.enum.of(...)` or pass a `ListSet`; a plain `Set` of 5 or more values iterates in hash order.
To match how a JSON library writes sealed families (play's `_type`, circe's wrapper object, ...), render with that
library's profile: see [Library profiles](#library-profiles).

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

`title` and `description` render on every kind of schema: objects, strings, numbers, arrays, maps, enums, `oneOf`s,
value classes. A definition's, and a field's `@title`/`@description` on a field whose type is a definition, sit next to
its `$ref` (before draft 2019-09 validators ignore keywords next to a `$ref`, so there they are for documentation
tools). A type's own annotations and scaladoc apply where it is the root, or where it is given as an implicit
`Schema` (a definition), and on the members of a sealed family; a type derived inline as a field's (or an element's)
type doesn't get them.

**Scaladoc.** The doc comment right before a class (annotations, modifiers and ordinary comments may sit in between)
gives its description, and its `@param` tags the field descriptions. A class without a doc comment gets none.
A `@param` for a field that doesn't exist is a warning. Where the comment comes from:

- **Scala 2:** by default only classes compiled in the same compiler run as the `Json.schema` call. A model from
  another module or a jar, or a file an incremental build didn't recompile, has no descriptions, unless the model's
  module is compiled with the [scaladoc](https://github.com/andyglow/scaladoc) compiler plugin: it embeds the
  scaladoc into the bytecode, and `Json.schema` reads it from there.
- **Scala 3:** the same by default. Add `scalacOptions += "-Yread-docs"` to the module that calls `Json.schema`
  (or `derives`), and the scaladoc is also read from the TASTy of other modules, jars and earlier incremental runs.
  This needs the model's module to keep its docs in TASTy, which is the Scala 3 default.

The [sbt-scaladoc](https://github.com/andyglow/scaladoc) plugin sets both up. On Scala 2 it adds the compiler plugin
(and `scaladoc-ast`, a small runtime dependency of the module); on Scala 3 it adds `-Yread-docs`:

```scala
// project/plugins.sbt
addSbtPlugin("io.github.andyglow" % "sbt-scaladoc" % "0.3.0")

// build.sbt: on the module that defines the models (Scala 2) and the one that calls Json.schema (Scala 3)
lazy val model = project.enablePlugins(ScaladocPlugin)
```

## Annotations

Package `json.schema`:

| annotation                                    | on                    | effect                                                        |
|-----------------------------------------------|-----------------------|---------------------------------------------------------------|
| `@title("...")`                               | type, field, enum value | `title`                                                     |
| `@description("...")`                         | type, field, enum value | `description`                                               |
| `@readOnly`                                   | field                 | `"readOnly": true`                                            |
| `@writeOnly`                                  | field                 | `"writeOnly": true`                                           |
| `@deprecated` (Scala's, or Java's `@Deprecated`) | field, type        | `"deprecated": true` (draft 2019-09 and 2020-12; not under the Legacy profile) |
| `@discriminator(field = "type", phantom = true)` | sealed root        | adds a discriminator field to each `oneOf` member             |
| `@discriminatorKey("key")`                    | sealed member         | the member's discriminator value (default: its full name)     |
| `@definition` / `@definition("name")`         | sealed member         | makes it a definition                                         |
| `@typeHint[T]`                                | sealed root, member   | the JSON type of enum values that come from a `ToValue`       |

A type's `@title`/`@description` apply where it is the root, a type given as an implicit `Schema`, or a member of a
sealed family. On Scala 2, a
field annotation can also target the underlying field: `@(description @field)("...")`.

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

## Any JSON value

`` json.Schema.`any`[T]() `` accepts any JSON value: it renders `{}` in every draft. Use it for a field whose content
you don't describe:

```scala
import com.typesafe.config.ConfigValue

implicit val configValuePredef: json.schema.Predef[ConfigValue] = json.schema.Predef(json.Schema.`any`[ConfigValue]())
```

The JSON libraries' own AST types get theirs from an import, one per bridge. Our own
`com.github.andyglow.json.Value` needs none.

| AST        | `{}` (any)    | free object  | array of any     | string, number, boolean, null                                         | import                                             |
|------------|---------------|--------------|------------------|-----------------------------------------------------------------------|----------------------------------------------------|
| ours       | `Value`       | `Value.obj`  | `Value.arr`      | `Value.str`, `Value.num`, `Value.bool`, `` Value.`null` ``            | none                                               |
| play-json  | `JsValue`     | `JsObject`   | `JsArray`        | `JsString`, `JsNumber`, `JsBoolean`                                   | `com.github.andyglow.jsonschema.AsPlay.predef._`   |
| circe      | `Json`        | `JsonObject` |                  | `JsonNumber`                                                          | `com.github.andyglow.jsonschema.AsCirce.predef._`  |
| ujson      | `ujson.Value` | `ujson.Obj`  | `ujson.Arr`      | `ujson.Str`, `ujson.Num`, `ujson.Bool`                                | `com.github.andyglow.jsonschema.AsU.predef._`      |
| json4s     | `JValue`      | `JObject`    | `JArray`, `JSet` | `JString`, `JDouble`, `JDecimal`, `JLong`, `JInt`, `JNumber`, `JBool` | `com.github.andyglow.jsonschema.AsJson4s.predef._` |
| spray-json | `JsValue`     | `JsObject`   | `JsArray`        | `JsString`, `JsNumber`, `JsBoolean`                                   | `com.github.andyglow.jsonschema.AsSpray.predef._`  |

```scala
import play.api.libs.json.{JsObject, JsValue}
import com.github.andyglow.jsonschema.AsPlay.predef._

case class Event(id: String, payload: JsValue, metadata: JsObject)

json.Json.schema[Event] // "payload": {}, "metadata": {"type": "object", "additionalProperties": true}
```

- The import is separate from `AsPlay._` (and the other bridges' `AsX._`), so a `Predef` of your own for one of these
  types doesn't become ambiguous. They are `Predef`s, so they are inlined. For a definition, declare
  `` implicit val s: json.Schema[JsValue] = json.Schema.`any`[JsValue]() ``.
- `Option[JsValue]` renders `{}` (it accepts `null` already), not `oneOf[{}, {"type": "null"}]`, which would reject `null`.
- A default value (`payload: JsValue = Json.obj()`) needs the bridge's `ToValue`: import `AsPlay._` (or
  `profiles.play._`) too, as for any other default.
- Don't put an `any` into a `oneOf`: it overlaps every other member, so the `oneOf` rejects their values.
- json4s leaves out a field whose value is `JNothing`, so a required `JValue` field then fails `required`. Make such a
  field an `Option[JValue]` or give it a default.
- Without the import, json4s' and spray's ASTs derive as families of their Scala classes (a JSON string would have to
  be `{"s": "..."}`), and the other libraries' don't compile.

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
Schemas for the library's own AST types (`JsValue`, `io.circe.Json`, `ujson.Value`, ...) come from a separate import,
`AsPlay.predef._`, `AsCirce.predef._`, `AsU.predef._`, `AsJson4s.predef._` or `AsSpray.predef._`: see [Any JSON value](#any-json-value).

### `Option`

Every `Option[X]` is "X or null": at the root, as a collection element, as a map value and as a field. An `Option`
field is also not required, and `Option[Option[X]]` is the same as `Option[X]`. This is what circe, play-json,
spray-json and upickle write (`None` is `null`, or the field is left out) and read back (upickle's reader wants
the key present unless the field has a default).

```scala
case class Pet(name: String, age: Option[Int], tags: List[Option[String]], owner: Option[Owner])
```

Draft-04, abridged (`Owner` has an implicit `Schema`, so it is a definition):

```json
{
  "type": "object",
  "properties": {
    "name": { "type": "string" },
    "age": { "type": ["integer", "null"] },
    "tags": { "type": "array", "items": { "type": ["string", "null"] } },
    "owner": { "oneOf": [{ "$ref": "#/definitions/Owner" }, { "type": "null" }] }
  },
  "required": ["name", "tags"]
}
```

Where X has one JSON type (a string, number, integer, boolean, array or object), the output is X with
`"type": [X, "null"]`. Otherwise (an enum, a `$ref`, a sealed family) it is `oneOf: [X, {"type": "null"}]`.

The macro keeps the fact "this is an `Option`" in the schema as an `` `option` `` node, and the renderer shapes it
with a `json.schema.JsonSchemaProfile`. The default profile is `JsonSchemaProfile.Neutral`, as above.
`JsonSchemaProfile.Legacy` renders exactly what 0.7 rendered: an `Option` field is X and not required, any other
`Option[X]` is `oneOf: [{"type": "string", "enum": ["None"]}, {"type": "object", "properties": {"value": X}, ...}]`. Use it for one
schema, or for every render where an implicit profile is in scope:

```scala
import json.schema.JsonSchemaProfile

Json.schema[Pet].withProfile(JsonSchemaProfile.Legacy)               // a json.Schema with the 0.7 shapes

implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy   // Scala 3: given JsonSchemaProfile = JsonSchemaProfile.Legacy
Json.stringify(Json.schema[Pet], Draft07("id"))                      // also AsValue.schema, asPlay, asCirce, ...
```

`withProfile` shapes the schema right away, so a later render keeps that shape whatever profile is in scope. To
share one profile across a project, define it in an object and import it where you render. In Scala 3, `import
Settings.*` doesn't import a `given`: write `import Settings.given`, or declare the profile as an `implicit val`.

### Library profiles

JSON libraries write sealed families differently. For

```scala
sealed trait Animal
final case class Dog(name: String) extends Animal
case object Fish                   extends Animal
```

play-json writes `{"_type": "petshop.Dog", "name": "Rex"}` and `{"_type": "petshop.Fish"}`, circe-generic
`{"Dog": {"name": "Rex"}}` and `{"Fish": {}}`, upickle `{"$type": "Dog", "name": "Rex"}` and `"Fish"`, json4s
`{"name": "Rex"}` and `{}`. The default schema (an untagged `oneOf`, case objects as a string enum, as in 0.7)
matches none of them. So the macro records the facts of a sealed family (its members, their names and kinds, the
annotations) in the schema as a `` `family` `` node, like `Option`, and a profile shapes it when you render.

One import gives rendering, `ToValue` and the library's profile (`AsPlay._`, `AsCirce._`, ... stay as they are
and bring no profile):

```scala
import com.github.andyglow.jsonschema.profiles.play._    // asPlay: what Json.format writes under the implicit JsonConfiguration in scope
import com.github.andyglow.jsonschema.profiles.circe._   // asCirce: circe-generic (semiauto); Scala 3: a given io.circe.derivation.Configuration in scope wins
import com.github.andyglow.jsonschema.profiles.upickle._ // asU: upickle.default
import com.github.andyglow.jsonschema.profiles.json4s._  // asJson4s: the implicit Formats in scope, else DefaultFormats
import com.github.andyglow.jsonschema.profiles.spray._   // asSpray: spray derives no sum codecs, so Neutral's shapes (state yours, below)
```

In Scala 3 write `.*`: the bridges declare `implicit def`s, which `.*` imports.

| library | as it writes `Dog` / `Fish` | profile | read from |
|---|---|---|---|
| play-json | `{"_type": "petshop.Dog", "name": ...}` / `{"_type": "petshop.Fish"}` | `PlayProfile.fromConfig(cfg)`, `PlayProfile.default` | `discriminator`, `typeNaming` (given the full name); `optionHandlers` needs nothing (play reads both) |
| circe | `{"Dog": {...}}` / `{"Fish": {}}`; configured: `{"type": "Dog", ...}` | `CirceProfile.default`, `CirceProfile.configured(discriminator, transformConstructorNames, transformMemberNames)`, Scala 3: `CirceProfile.fromConfig(configuration)` | `discriminator`, `transformConstructorNames` |
| upickle | `{"$type": "Dog", "name": ...}` / `"Fish"` | `UPickleProfile.fromApi(api)`, `UPickleProfile.default` | `tagName`, `objectTypeKeyWriteFullyQualified`, `objectTypeKeyWriteMap`; an `Option` key must be present unless the field has a default |
| json4s | `{"name": ...}` / `{}`; with type hints `{"jsonClass": "Dog", ...}` | `Json4sProfile.fromFormats(formats)`, `Json4sProfile.default` | `typeHints`, per family: a family of hinted classes is tagged, one without hints isn't |
| spray-json | your `RootJsonFormat` | `SprayProfile.default` (Neutral) | nothing: say what your format writes |

Where the library takes its configuration implicitly, the profile follows the same implicit, so a custom
configuration changes the schema with no extra code:

```scala
import play.api.libs.json._
import com.github.andyglow.jsonschema.profiles.play._
import json.schema.Version._

implicit val cfg: JsonConfiguration = JsonConfiguration(
  discriminator  = "_kind",
  typeNaming     = JsonNaming(_.split('.').last),           // "petshop.Dog" -> "Dog"
  optionHandlers = OptionHandlers.WritesNull
)
implicit val animalF: OFormat[Animal] = Json.format[Animal]   // and Dog, Fish, Pet; play's macros read `cfg`

json.Json.schema[Pet].asPlay(Draft12("pet"))                // the same `cfg`: "_kind", "Dog", "age": ["integer", "null"]
```

The same holds for json4s (`implicit val formats: Formats = DefaultFormats + ShortTypeHints(List(classOf[Dog],
Fish.getClass), "kind")`) and, in Scala 3, circe's `given Configuration = Configuration.default.withDiscriminator("type")`
next to `derives ConfiguredCodec`. Where the configuration is not an implicit, pass it:

```scala
// upickle: your API object
object MyPickler extends upickle.AttributeTagged { override def tagName = "kind" }
implicit val profile: JsonSchemaProfile = UPickleProfile.fromApi(MyPickler)

// circe-generic-extras (Scala 2): the fields of its Configuration that shape the output
implicit val profile: JsonSchemaProfile =
  CirceProfile.configured(config.discriminator, config.transformConstructorNames, config.transformMemberNames)

// spray: what your hand-written RootJsonFormat[Animal] writes
implicit val profile: JsonSchemaProfile =
  SprayProfile.default.withSum(SumStyle.Discriminator("type")).withMemberName(MemberName.SimpleName).withSingleton(SingletonStyle.AsTagged)
```

With a profile of your own, import `AsX._` (rendering, no profile), not `profiles.<lib>._`. With both in scope,
Scala 2 reports the ambiguity with a message saying so; Scala 3 picks yours, the closer one. Two `profiles.<lib>._`
imports in one scope are an error too. Per call, `schema.withProfile(PlayProfile.fromConfig(cfg))` shapes one
schema whatever is in scope where it is rendered.

What the profiles can't see: the libraries' own annotations (upickle `@key`, circe-generic-extras `@JsonKey`, play's
`@JsonKey`) and hand-written codecs (give such a type an implicit `Schema`). Renamed fields (play `naming`, circe
`transformMemberNames`, upickle `objectAttributeKeyWriteMap`) are an error (tried on sample names). json4s 4 fails
to encode case classes on Scala 3 itself, so its profile is for Scala 2 users. A `Map` whose key play-json has no
`KeyWrites` for (e.g. `Map[UUID, V]`) is written by play as an array of pairs, which `PlayProfile` doesn't describe
(see [Supported types](#supported-types)).

What a profile describes (the same rules for every library):
- what the library writes, widened only by what its reader also accepts (play reads a missing `Option` field and
  `null`, so both validate). Value classes are unwrapped (as fields, collection elements, ...); a value class that
  is a member of a sealed family is the case class `{"<field>": value}`, as the libraries' derived sum codecs
  write it;
- a member's tag (`_type`, `$type`, ...) and name come from the library's configuration, not from
  `@discriminator`/`@discriminatorKey`: the library's derivation never sees those. They apply under Neutral and
  Legacy, and under the spray profile, where you write the sum codec yourself. `@title`, `@description` and
  `@definition` apply under every profile;
- a setting the profile can't describe is an error when the profile is built (or the schema is lowered), naming
  the setting, not a schema that rejects the library's own output;
- a type with an implicit `Schema` or `Predef` is used as it is. A family member with an implicit `Schema` of an
  object gets the tag in a copy of it; one whose schema isn't an object can't carry a tag, and that is an error;
- nested sealed traits are flattened, as the libraries do; `Either` is not a family (still the 0.7 `oneOf`).

A profile of your own starts from one of the library profiles, or from `JsonSchemaProfile.Neutral`, and changes
knobs; each `with...` keeps the others:

| knob | values | |
|---|---|---|
| `withSum` | `SumStyle.Untagged`, `WrapperObject`, `Discriminator(field)`, `PerFamily(family => style)` | how members are told apart |
| `withSingleton` | `SingletonStyle.AsString`, `AsTagged` | a case object as a string, or as an object tagged like the other members |
| `withMemberName` | `MemberName.SimpleName`, `FullName`, `Custom(member => name)` | the tag of a member |
| `withOptionKeyRequired` | `true`, `false` | an `Option` field without a default is required (upickle) |
| `withHonorsSchemaAnnotations` | `true`, `false` | whether `@discriminator`/`@discriminatorKey` apply |
| `withOption` | `OptionStyle.Nullable`, `Legacy` | `Option` as "X or null", or the 0.7 shapes |
| `withOverlappingMembers` | `OverlappingMembers.AnyOf`, `OneOf` | an untagged family where one document could match two members: `anyOf`, or `oneOf` anyway (it then rejects that document) |
| `withDeprecatedKeyword` | `true`, `false` | whether `@deprecated` renders `"deprecated": true` (`false` only for `Legacy`) |

```scala
import json.schema.JsonSchemaProfile
import json.schema.JsonSchemaProfile.{MemberName, SingletonStyle, SumStyle}

// a hand-written codec that writes {"type": "Dog", "name": ...} and {"type": "Fish"}
implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Neutral
  .withSum(SumStyle.Discriminator("type"))
  .withMemberName(MemberName.SimpleName)
  .withSingleton(SingletonStyle.AsTagged)

Json.stringify(Json.schema[Animal], Draft12("animal"))
```

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

Supported: `NonEmptyList`, `NonEmptyVector`, `NonEmptySet`, `NonEmptyChain`, `NonEmptySeq`, `NonEmptyMap`, `OneAnd`,
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
(`time`), `Duration` (`duration`), `Period` (`string` with a `pattern` for its `toString` form, e.g. `P1Y2W3DT-4H5.5S`),
`DateTimeZone` and `Interval` (`string`). They are implicit `Schema`s, so they appear as definitions. To inline them
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
subset, including `"type": [X, "null"]`) into a `Try[json.Schema[_]]`. `{}` (or an object of only annotations such as
`title`) is `` Schema.`any` ``. The boolean schemas `true` and `false` (draft-06+, read in a draft-04 document too) are
`any` and `not(any)` in `items` and `properties`.

## Migrating to 0.9.0

The full list is in the [CHANGELOG](CHANGELOG.md).

**Output**

- Sealed families render as before unless you ask for a library profile: the default profile (Neutral) and `JsonSchemaProfile.Legacy` render
  sealed families exactly as 0.8 and 0.7 did. To get a schema that matches your codec, import your library's
  profile (see [Library profiles](#library-profiles)).
- Sealed family members are in declaration order on Scala 2 too (the `family` node; a rendered `oneOf` or `enum`
  keeps it up to 4 entries, with more the order is fixed hash order). Before, families of 5 and more members could
  come out in a different order from one build to the next. Members of a family compiled in another project (no
  source positions) are in name order.
- Two renderer bugs are fixed, also under Legacy: a `oneOf` with a definition or a `$ref` after its first member
  threw on draft-04/06/07, and a `const` (`Flag.EnumsAsOneOf`) could render `"type": "const"`, which no document
  matches.

**API**

- **The derived tree has a `` `family` `` node** for each sealed family (and Scala 3 `enum`), where it had a
  `oneof` (or an `enum` for case objects only). Code that walks or compares derived trees sees it.
  `schema.withProfile(JsonSchemaProfile.Neutral)` (or `Legacy`) gives the plain tree, the same as 0.8's.
- **`object.Field` has a 7th parameter, `hasDefault`**: the Scala field has a default, also when it has no JSON form
  (`= None`, or no `ToValue` in scope). `new Field(...)` and `Field(...)` with 6 arguments still compile; a pattern
  `case Field(name, tpe, required, default, description, rwMode)` needs a 7th binder (or match `f: Field[_]` and
  use its accessors).

## Migrating to 0.8.0

The full list is in the [CHANGELOG](CHANGELOG.md).

**Build**

- The groupId is `io.github.andyglow` (was `com.github.andyglow`). Imports don't change.
- Scala 2.11 is no longer published. Scala 3.3 is.
- Bridges and modules are built against current library versions, notably play-json 3 (`org.playframework`).

**Output that changes**

- **Every `Option[X]` is "X or null"** (see [`Option`](#option)). An `Option` field was X, so `"o": null` (what
  circe and upickle write by default) didn't validate; it is now `"type": [X, "null"]`, still not required. An
  `Option` at the root, in a collection or as a map value was `oneOf` of `{"value": X}` and `"None"`, which no JSON
  library writes. `Option[Option[X]]` is the same as `Option[X]`. To get the 0.7 output back, render with
  `JsonSchemaProfile.Legacy`: one implicit where you render
  (`implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy`), or `schema.withProfile(JsonSchemaProfile.Legacy)`.
  Legacy reproduces 0.7 byte for byte.
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

- **The derived `json.Schema` keeps `Option` as an `` `option` `` node.** An `Option` field is
  `Field("o", option(integer), required = false)` (was `integer`), and an `Option` elsewhere is `option(X)` (was
  `oneof` of `Some`/`None`). This matters if you walk the tree yourself (your own renderer, `withFieldsUpdated` on
  `f.tpe`, comparing with a hand-written `json.Schema` in tests). `schema.withProfile(JsonSchemaProfile.Legacy)`
  gives the 0.7 tree. The model also has a `` `null` `` leaf (`{"type": "null"}`).
- The renderers (`AsValue.schema`, `Json.stringify`, `asPlay`, `asCirce`, ...) take an implicit
  `JsonSchemaProfile`. Nothing changes at the call site: without one in scope, `JsonSchemaProfile.Neutral` is used.
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
  `ParseJsonSchema` reads `"type": "null"` and `"type": [X, "null"]` (as `oneof{X, null}`), so the schemas this
  version renders for `Option` fields parse.

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
  schema, an implicit `Schema` for an abstract type parameter is used as it is, as for a generic implicit `def` on
  both versions.
- **refined:** `refined[T]` has no `T <: Refined[_, _]` bound (Refined is an opaque type on Scala 3; a non-refined
  `T` is a compile error), and an unsupported predicate found while deriving fails with the derivation's
  `is not supported` error followed by refined's own message.
- **`*:` tuples** (`Int *: String *: EmptyTuple`, what `Tuple.Concat`/`Tuple.Map` produce) are the same object as
  `(Int, String)`, also beyond 22 elements (`_1` .. `_23` ...). `Tuple`, `NonEmptyTuple`, `Int *: Tuple` (no known
  arity) and `EmptyTuple` are compile errors.
- Compile errors quote Scala 3 code, so their text differs from Scala 2's.

To see the code the macro generates, compile with `-Djsonschema.print-code=true` (e.g.
`sbt -Djsonschema.print-code=true compile`): every expansion is printed as an info message.

Not supported (yet): union types, literal types, opaque types (beyond refined) and named tuples.

## Known issues

- **Recursive generic definitions.** For `case class Tree[A](value: A, children: List[Tree[A]])` with a generic
  implicit (`implicit def treeSchema[A: json.Schema]: json.Schema[Tree[A]] = Json.schema[Tree[A]]`, or on Scala 3
  `derives json.Schema`), `Tree[Company]` used as a field or as the root of `Json.schema` is a definition named
  `pkg.Tree[pkg.Company]`. But the definition keeps the type parameter's name (e.g. `pkg.Tree[pkg.Tree.A]`)
  when it is a `Tree[A]` inside another generic implicit (`case class Outer[A](tree: Tree[A])` with its own
  `implicit def`), or comes from a generic class's member
  (`class Wrap[T: json.Schema] { implicit val s: json.Schema[Tree[T]] = Json.schema[Tree[T]] }`).
  Two instantiations in one schema then get **one definition with one body: the schema is invalid**. A summoned
  `Schema[Tree[X]]` (`implicitly`, `summon`) keeps that name too (valid on its own). Workaround: one `implicit val`
  per instantiation (`implicit val companyTree: json.Schema[Tree[Company]] = Json.schema[Tree[Company]]`).
- **Scala 2.13 before 2.13.17: `-Xlint` warns "Implicit resolves to enclosing value"** on
  `implicit val s: Schema[X] = Json.schema[X]`. The derivation doesn't use `s` (it derives X), but these compilers can't
  leave `s` out of the macro's implicit search, and the lint reports the search. Use Scala 2.13.17 or later (sbt
  requires 2.13.18 with this library anyway), or add `-Wconf:cat=w-flag-self-implicit:s`.
- Tuples are objects (`{"_1": ..., "_2": ...}`) and `Either[L, R]` is a `oneOf` of `{"value": L}` and `{"value": R}`.
- On draft-04, `exclusiveMinimum`/`exclusiveMaximum` are rendered as numbers (draft-06 style).

## Documentation site

The site sources are in `docs/src/main/paradox`. Build it with `sbt makeDocs` (output in `docs/target/site`).
