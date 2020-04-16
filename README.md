# Scala JSON Schema
[![Build Status](https://travis-ci.org/andyglow/scala-jsonschema.svg)](https://travis-ci.org/andyglow/scala-jsonschema)
[![Coverage Status](https://coveralls.io/repos/github/andyglow/scala-jsonschema/badge.svg?branch=master)](https://coveralls.io/github/andyglow/scala-jsonschema?branch=master)
![](https://img.shields.io/static/v1.svg?label=Versions&message=-->&color=gray)
[![mvn: 2.11](https://img.shields.io/badge/dynamic/json.svg?label=mvn%3A%202.11&query=%24.response.docs%5B0%5D.latestVersion&url=https%3A%2F%2Fsearch.maven.org%2Fsolrsearch%2Fselect%3Fq%3Dscala-jsonschema-core_2.13%26start%3D0%26rows%3D1)](https://search.maven.org/artifact/com.github.andyglow/scala-jsonschema-core_2.11/)
[![mvn: 2.12](https://img.shields.io/badge/dynamic/json.svg?label=mvn%3A%202.12&query=%24.response.docs%5B0%5D.latestVersion&url=https%3A%2F%2Fsearch.maven.org%2Fsolrsearch%2Fselect%3Fq%3Dscala-jsonschema-core_2.13%26start%3D0%26rows%3D1)](https://search.maven.org/artifact/com.github.andyglow/scala-jsonschema-core_2.12/)
[![mvn: 2.13](https://img.shields.io/badge/dynamic/json.svg?label=mvn%3A%202.13&query=%24.response.docs%5B0%5D.latestVersion&url=https%3A%2F%2Fsearch.maven.org%2Fsolrsearch%2Fselect%3Fq%3Dscala-jsonschema-core_2.13%26start%3D0%26rows%3D1)](https://search.maven.org/artifact/com.github.andyglow/scala-jsonschema-core_2.13/)

*SBT dependencies:*

Main module:

```scala
libraryDependencies += "com.github.andyglow" %% "scala-jsonschema" % <version> // <-- required
```

Other libraries:

```scala
libraryDependencies ++= Seq(
  "com.github.andyglow" %% "scala-jsonschema-core" % <version>,              // <-- transitive
  "com.github.andyglow" %% "scala-jsonschema-macros" % <version> % Provided, // <-- transitive
  // json bridge. pick one
  "com.github.andyglow" %% "scala-jsonschema-play-json" % <version>,         // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-spray-json" % <version>,        // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-circe-json" % <version>,        // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-json4s-json" % <version>,       // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-ujson" % <version>,             // <-- optional
  // joda-time support
  "com.github.andyglow" %% "scala-jsonschema-joda-time" % <version>,         // <-- optional
  // cats support
  "com.github.andyglow" %% "scala-jsonschema-cats" % <version>,              // <-- optional
  // refined support
  "com.github.andyglow" %% "scala-jsonschema-refined" % <version>,           // <-- optional
  // zero-dependency json and jsonschema parser
  "com.github.andyglow" %% "scala-jsonschema-parser" % <version>             // <-- optional
)
```


## Generate JSON Schema from Scala classes

The goal of this library is to make JSON Schema generation done the way all popular JSON reading/writing libraries do.
Inspired by Coursera Autoschema but uses `Scala Macros` instead of `Java Reflection`.

## Features
- Supports Json Schema `draft-04`, `draft-06`, `draft-07`
- Supports `value classes`
- Supports `sealed trait enums`
- Supports `sealed trait case classes`
- Treats `Option` as optional fields
- As well as treats fields with `default values` as optional
- Any `Iterable` will be treated as `array`
- Pluggable Joda-Time Support
- Pluggable Cats Support
- Supports generic data types

### Types supported out of the box
- `Boolean`
- Numeric
    - `Short`
    - `Int`
    - `Char`
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
    - with JodaTime module imported
        - `org.joda.time.Instant`
        - `org.joda.time.DateTime`
        - `org.joda.time.LocalDateTime`
        - `org.joda.time.LocalDate`
        - `org.joda.time.LocalTime`
    - with Cats module imported
        - `cats.data.NonEmptyList`
        - `cats.data.NonEmptyVector`
        - `cats.data.NonEmptySet`
        - `cats.data.NonEmptyChain`
        - `cats.data.NonEmptyMap`
        - `cats.data.NonEmptyStream` (for scala 2.11, 2.12)
        - `cats.data.NonEmptyLazyList` (for scala 2.13)
        - `cats.data.OneAnd`
    - with Refined module imported you can refine original types wiith these
        - boolean
            - `eu.timepit.refined.boolean.And`
            - `eu.timepit.refined.boolean.Or`
            - `eu.timepit.refined.boolean.Not`
        - string
            - `eu.timepit.refined.collection.Size`
            - `eu.timepit.refined.collection.MinSize`
            - `eu.timepit.refined.collection.MaxSize`
            - `eu.timepit.refined.collection.Empty`
            - `eu.timepit.refined.string.Uuid`
            - `eu.timepit.refined.string.Uri`
            - `eu.timepit.refined.string.Url`
            - `eu.timepit.refined.string.IPv4`
            - `eu.timepit.refined.string.IPv6`
            - `eu.timepit.refined.string.Xml`
            - `eu.timepit.refined.string.StartsWith`
            - `eu.timepit.refined.string.EndsWith`
            - `eu.timepit.refined.string.MatchesRegex`
        - number
            - `eu.timepit.refined.numeric.Positive`
            - `eu.timepit.refined.numeric.Negative`
            - `eu.timepit.refined.numeric.NonPositive`
            - `eu.timepit.refined.numeric.NonNegative`
            - `eu.timepit.refined.numeric.Greather`
            - `eu.timepit.refined.numeric.Less`
            - `eu.timepit.refined.numeric.GreaterEqual`
            - `eu.timepit.refined.numeric.LessEqual`
            - `eu.timepit.refined.numeric.Divisable`
        - collection
            - `eu.timepit.refined.collection.Size`
            - `eu.timepit.refined.collection.MinSize`
            - `eu.timepit.refined.collection.MaxSize`
            - `eu.timepit.refined.collection.Empty`
            
- Misc
    - `java.util.UUID`
    - `java.net.URL`
    - `java.net.URI`
- Collections
    - String Map (eg. `Map[String, T]`)
    - Int Map (eg. `Map[Int, T]`)
    - `Iterable[T]`
- Sealed Trait hierarchy of case objects (Enums)
- Case Classes
    - default value
- Sealed Trait hierarchy of case classes
- Value Classes    

## Example
Suppose you have defined this data structures
```scala
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
```scala
import json._

val personSchema: json.Schema[Person] = Json.schema[Person]
```
As result you will receive this:
```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "middleName": {
      "type": "string"
    },
    "cars": {
      "type": "array",
      "items": {
        "type": "object",
        "additionalProperties": false,
        "properties": {
          "name": {
            "type": "string"
          },
          "manufacturer": {
            "type": "object",
            "additionalProperties": false,
            "properties": {
              "name": {
                "type": "string"
              }
            },
            "required": [
              "name"
            ]
          }
        },
        "required": [
          "name",
          "manufacturer"
        ]
      }
    },
    "company": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "name": {
          "type": "string"
        }
      },
      "required": [
        "name"
      ]
    },
    "lastName": {
      "type": "string"
    },
    "firstName": {
      "type": "string"
    },
    "birthDay": {
      "type": "string",
      "format": "date-time"
    },
    "gender": {
      "type": "string",
      "enum": [
        "Male",
        "Female"
      ]
    }
  },
  "required": [
    "company",
    "lastName",
    "birthDay",
    "gender",
    "firstName",
    "cars"
  ]
}
```

### Regular
Schema generated in Regular mode will contain so many `definitions` so many separated definitions you provide.
Lets take a look at example code: 

```scala
import json._

implicit val genderSchema: json.Schema[Gender] = Json.schema[Gender]

implicit val companySchema: json.Schema[Company] = Json.schema[Company]

implicit val carSchema: json.Schema[Car] = Json.schema[Car]

implicit val personSchema: json.Schema[Person] = Json.schema[Person]
```
Here we defined, besides Person schema, gender, company and car schemas. 
The result will be looking this way then.
```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "middleName": {
      "type": "string"
    },
    "cars": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Car"
      }
    },
    "company": {
      "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Company"
    },
    "lastName": {
      "type": "string"
    },
    "firstName": {
      "type": "string"
    },
    "birthDay": {
      "type": "string",
      "format": "date-time"
    },
    "gender": {
      "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Gender"
    }
  },
  "required": [
    "company",
    "lastName",
    "birthDay",
    "gender",
    "firstName",
    "cars"
  ],
  "definitions": {
    "com.github.andyglow.jsonschema.ExampleMsg.Company": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "name": {
          "type": "string"
        }
      },
      "required": [
        "name"
      ]
    },
    "com.github.andyglow.jsonschema.ExampleMsg.Car": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "name": {
          "type": "string"
        },
        "manufacturer": {
          "$ref": "#/definitions/com.github.andyglow.jsonschema.ExampleMsg.Company"
        }
      },
      "required": [
        "name",
        "manufacturer"
      ]
    },
    "com.github.andyglow.jsonschema.ExampleMsg.Gender": {
      "type": "string",
      "enum": [
        "Male",
        "Female"
      ]
    }
  }
}
```

## Definitions/References
There are couple of ways to specify reference of schema.
1. It could be generated from type name (including type args)
2. You can do it yourself. It is useful when you want to provide couple of schemas with same type but with different validation rules.

So originally you use
```scala
import json._

implicit val someStrSchema: json.Schema[String] = Json.schema[String]

implicit val someArrSchema: json.Schema[Array[String]] = Json.schema[Array[String]]

println(JsonFormatter.format(AsValue.schema(someArrSchema)))
``` 

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "array",
  "items": {
    "$ref": "#/definitions/java.lang.String"
  },
  "definitions": {
    "java.lang.String": {
      "type": "string"
    }
  }
}
```

See that `java.lang.String`?

To use custom name, just apply it.
```scala
import json._

implicit val someStrSchema: json.Schema[String] = Json.schema[String]("my-lovely-string")

implicit val someArrSchema: json.Schema[Array[String]] = Json.schema[Array[String]]

println(JsonFormatter.format(AsValue.schema(someArrSchema, json.schema.Version.Draft04())))
``` 

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "array",
  "items": {
    "$ref": "#/definitions/my-lovely-string"
  },
  "definitions": {
    "my-lovely-string": {
      "type": "string"
    }
  }
}
```

There is, though, one circumstance that will make you think twice defining `implicit val someStrSchema: json.Schema[String] = Json.schema[String]` as it will influence all string fields or components of your schema.
Say you want to use simple string along with validated string for ID representation.
As the library operates at compile time level it completely rely on type information and
thus it limits us to only one solution: specify special types as types.

### Use Value Classes.
```scala
case class UserId(value: String) extends AnyVal

case class User(id: UserId, name: String)
```

Then you can do
```scala
import json._

implicit val userIdSchema: json.Schema[UserId] = Json.schema[UserId]("userId")

implicit val userSchema: json.Schema[User] = Json.schema[User]

println(JsonFormatter.format(AsValue.schema(someArrSchema)))
``` 

and expect
```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "id": {
      "$ref": "#/definitions/userId"
    },
    "name": {
      "type": "string"
    },
    "required": [
      "id",
      "name"
    ],
    "definitions": {
      "userId": {
        "type": "string"
      }
    }
  }
}
``` 

## Validation
It is also possible to add specific validation rules to our schemas.

Available validations:
- multipleOf
- maximum
- minimum
- exclusiveMaximum
- exclusiveMinimum
- maxLength
- minLength
- pattern
- maxItems
- minItems
- uniqueItems
- maxProperties
- minProperties

Example
```scala
import json._
import json.Validation._

implicit val vb = ValidationBound.mk[UserId, String]

implicit val userIdSchema: json.Schema[UserId] = Json.schema[UserId]("userId") withValidation (
  `pattern` := "[a-f\\d]{16}"
)
``` 
Definition will look then like
```json
{
  "userId": {
    "type": "string",
    "pattern": "[a-f\\d]{16}"
  }
}
```

## Joda Time
Joda Time Support allows you to use joda-time classes within your models.
Here is an example.
 
```scala
import com.github.andyglow.jsonschema.JodaTimeSupport._
import org.joda.time._

case class Event(id: String, timestamp: Instant)

val eventSchema: Schema[Event] = Json.schema[Event]

println(JsonFormatter.format(AsValue.schema(eventSchema)))
```
results in
```
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "id": {
      "type": "string"
    },
    "timestamp": {
      "$ref": "#/definitions/org.joda.time.Instant"
    }
  },
  "required": [
    "id",
    "timestamp"
  ],
  "definitions": {
    "org.joda.time.Instant": {
      "type": "string",
      "format": "date-time"
    }
  }
}
```

## Json Libraries
The library uses its own Json model _com.github.andyglow.json.Value_ to represent Json Schema as JSON document.
But project contains additionally several modules which could connect it with library of your choice.

Currently supported:
- Play Json
- Spray Json
- Circe
- Json4s
- uJson

Example usage: _Play_
```scala
import com.github.andyglow.jsonschema.AsPlay._
import json.schema.Version._
import play.api.libs.json._

case class Foo(name: String)

val fooSchema: JsValue = Json.schema[Foo].asPlay(Draft04())
``` 

Example usage: _Spray_
```scala
import com.github.andyglow.jsonschema.AsSpray._
import json.schema.Version._
import spray.json._

case class Foo(name: String)

val fooSchema: JsValue = Json.schema[Foo].asSpray(Draft04())
``` 

Example usage: _Circe_
```scala
import com.github.andyglow.jsonschema.AsCirce._
import json.schema.Version._
import io.circe._

case class Foo(name: String)

val fooSchema: Json = Json.schema[Foo].asCirce(Draft04())
``` 

Example usage: _Json4s_
```scala
import com.github.andyglow.jsonschema.AsJson4s._
import json.schema.Version._
import org.json4s.JsonAST._

case class Foo(name: String)

val fooSchema: JValue = Json.schema[Foo].asJson4s(Draft04())
``` 

Example usage: _uJson_
```scala
import com.github.andyglow.jsonschema.AsU._
import json.schema.Version._

case class Foo(name: String)

val fooSchema: ujson.Value = Json.schema[Foo].asU(Draft04())
``` 

## TODO
- support of self-referenced case classes
- support for case classes defined locally (problem comes from inability to locate companion in this case)