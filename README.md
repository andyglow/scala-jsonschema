# AutoSchema

This is a fork of https://github.com/coursera/autoschema that merges several sanity updates
and updates some features to be of use with the current status of JSON schemas.

This fork is published on maven central. See Installation section for instructions.

## Generate JSON Schema from Scala classes!

Features

* Generates JSON Schema from classes
* Annotate your classes to customize schema generation
* Supports common types
    * Seq, Lists, Arrays, etc.
    * Options
    * java.util.Date and Joda DateTime
    * String, Boolean, Int, Long, Double
    * java.util.UUID
* Caching of generated schema

Unsupported Features

* Play Framework JSON types

## Installation
Add this line to your `build.sbt`:

        libraryDependencies += "com.sauldhernandez" %% "autoschema" % "1.0.3"

## Usage
With a type parameter

        case class MyType(myValue: Int)
        AutoSchema.createSchema[MyType]

With a reflection type

        AutoSchema.createSchema(myReflectionType)

## Annotations
AutoSchema has a few annotations that you can use to customize generated schema

### Description
Lets you manually define comments for case clases and fields which are translated in a description field
of the commented element in the JSON schema.

        @Description("This is my class")
        case class MyClass(@Term.Description("This is my field") myField: String)

### FormatAs
Lets you manually set the type and format values of the schema to be generated for a specific type or value

#### On a Type
        @FormatAs("string", "date")
        case class MyDateType

#### On a Value
        case class MyType(@Term.FormatAs("string", "date") date: String)

### ExposeAs
Lets you use the schema of some type as the schema for another type or value

#### On a Type
        @ExposeAs[Int]
        case class MyIntLikeType

#### On a Value
        case class MyType(@Term.ExposeAs[Id] id: MyTypeId)

### Hide
Lets you hide a value in generated schema

        case class MyType(@Term.Hide mySecretField: Int)

Copyright 2014 Coursera Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
