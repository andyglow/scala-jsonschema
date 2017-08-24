# Scala JSON Schema
[![Build Status](https://travis-ci.org/andyglow/scala-jsonschema.svg)](https://travis-ci.org/andyglow/scala-jsonschema)
[![Maven Central 2.11](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.11)
[![Maven Central 2.12](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.12)
[![Bintray](https://api.bintray.com/packages/andyglow/scala-tools/scala-jsonschema/images/download.svg) ](https://bintray.com/andyglow/scala-tools/scala-jsonschema/_latestVersion)
[![Coverage Status](https://coveralls.io/repos/andyglow/scala-jsonschema/badge.svg)](https://coveralls.io/r/andyglow/scala-jsonschema)

## Generate JSON Schema from Scala classes

The goal of this library is to make JSON Schema generation done the way all popular JSON reading/writing libraries do.
Inspired by Coursera Autoschema but uses `Scala Macros` instead of `Java Reflection`.

## Features

- Generate Json Schema
- Treat `Option` as optional fields
- As well as treat fields with `default values` as optional
- Support `value classes`
- Support `sealed trait enums`
- Any `Traversable` will be treated as `array`

### Types supported out of the box
- `Boolean`
- Numeric
    - `Short`
    - `Int`
    - `Double`
    - `Float`
    - `Long`
    - `BigInt`
    - `BigDecimal`
- `String`
- Date Time
    - `java.util.Date`
    - `java.sql.Timestamp`
    - `java.time.Instant`
    - `java.time.LocalDateTime`
    - `java.sql.Date`
    - `java.time.LocalDate`
    - `java.sql.Time`
    - `java.time.LocalTime`
- Misc
    - `java.util.UUID`
    - `java.net.URL`
    - `java.net.URI`
- Collections
    - String Map (eg. `Map[String, T]`)
    - Int Map (eg. `Map[Int, T]`)
    - `Traversable[T]`
- Sealed Trait hierarchy of case objects (Enums)
- Case Classes
- Value Classes    

## Example
Suppose you have defined this data structures
```
sealed trait Gender

object Gender {

    case object Male extends Gender

    case object Female extends Gender
}

case class Company(name: String)

case class Car(name: String, manufacturer: Company)

case class Person(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    gender: Gender,
    birthDay: java.time.LocalDateTime,
    company: Company,
    cars: Seq[Car])
```

Now you have several ways to specify your schema.

### In-Lined
In simple words in-lined mode means you will have no `definitions`. Type you want to use as source for schema will
be represented in json schema without reusable data blocks.  
```
val personSchema: json.Schema[Person] = Json.schema[Person]
```
As result you will receive this:
```
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "middleName": {
      "type": "string",
    },
    "cars": {
      "type": "array",
      "items": {
        "type": "object",
        "additionalProperties": false,
        "properties": {
          "name": {
            "type": "string",
          },
          "manufacturer": {
            "type": "object",
            "additionalProperties": false,
            "properties": {
              "name": {
                "type": "string",
              },
            },
            "required": [
              "name",
            ],
          },
        },
        "required": [
          "name",
          "manufacturer",
        ],
      },
    },
    "company": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "name": {
          "type": "string",
        },
      },
      "required": [
        "name",
      ],
    },
    "lastName": {
      "type": "string",
    },
    "firstName": {
      "type": "string",
    },
    "birthDay": {
      "type": "string",
      "format": "date-time",
    },
    "gender": {
      "type": "string",
      "enum": [
        "Male",
        "Female",
      ],
    },
  },
  "required": [
    "company",
    "lastName",
    "birthDay",
    "gender",
    "firstName",
    "cars",
  ],
}
```

### Regular
Schema generated in Regular mode will contain so many `definitions` so many separated definitions you provide.
Lets take a look at example code: 

```
implicit val genderSchema: json.Schema[Gender] = Json.schema[Gender]

implicit val companySchema: json.Schema[Company] = Json.schema[Company]

implicit val carSchema: json.Schema[Car] = Json.schema[Car]

implicit val personSchema: json.Schema[Person] = Json.schema[Person]
```
Here we defined, besides Person schema, gender, company and car schemas. 
The result will be looking this way then.
```
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "middleName": {
      "type": "string",
    },
    "cars": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Car",
      },
    },
    "company": {
      "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Company",
    },
    "lastName": {
      "type": "string",
    },
    "firstName": {
      "type": "string",
    },
    "birthDay": {
      "type": "string",
      "format": "date-time",
    },
    "gender": {
      "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Gender",
    },
  },
  "required": [
    "company",
    "lastName",
    "birthDay",
    "gender",
    "firstName",
    "cars",
  ],
  "definitions": {
    "com.github.andyglow.jsonschema.ExampleMsg.Company": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "name": {
          "type": "string",
        },
      },
      "required": [
        "name",
      ],
    },
    "com.github.andyglow.jsonschema.ExampleMsg.Car": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "name": {
          "type": "string",
        },
        "manufacturer": {
          "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Company",
        },
      },
      "required": [
        "name",
        "manufacturer",
      ],
    },
    "com.github.andyglow.jsonschema.ExampleMsg.Gender": {
      "type": "string",
      "enum": [
        "Male",
        "Female",
      ],
    },
  },
}
```

## Json
The library uses its own Json model to represent Json Schema as JSON document.
But project contains additionally several modules which could connect it with library of your choice.

Currently supported:
- Play Json
- Spray Json
- Circe

Example usage: _Play_
```
import con.github.andyglow.jsonschema.AsPlay._
import play.api.libs.json._

case class Foo(name: String)

val feeSchema: JsValue = Json.schema[Foo].asPlay()
``` 

Example usage: _Spray_
```
import con.github.andyglow.jsonschema.AsSpray._
import spray.json._

case class Foo(name: String)

val feeSchema: JsValue = Json.schema[Foo].asSpray()
``` 

Example usage: _Circe_
```
import con.github.andyglow.jsonschema.AsCirce._
import io.circe._

case class Foo(name: String)

val feeSchema: Json = Json.schema[Foo].asCirce()
``` 

## TODO
- support of self-references
- decorations ("multipleOf", minimum, exclusiveMinimum, maximum, exclusiveMaximum, format, pattern, etc..)
