# Overview

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
- Pluggable Refined Support
- Supports generic data types            

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