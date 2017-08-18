# Scala JSON Schema
[![Build Status](https://travis-ci.org/andyglow/scala-jsonschema.svg)](https://travis-ci.org/andyglow/scala-jsonschema)
[![Maven Central 2.11](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.11)
[![Maven Central 2.12](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.andyglow/scala-jsonschema_2.12)
[![Bintray](https://api.bintray.com/packages/andyglow/scala-tools/scala-jsonschema/images/download.svg) ](https://bintray.com/andyglow/scala-tools/scala-jsonschema/_latestVersion)

The goal of this library is to make JSON Schema generation done the way all popular JSON reading/writing libraries do.
Using Scala Macros.

Example:
```
import json._

case class Address(street1: String, street2: Option[String])

val addressSchema = Schema[Address]

println(addressSchema.asJson.rendered)
```

this script will generate you JSON representation of Schema of this form
```
{
    "type": "object",
    "required": [ "street1" ],
    "properties": {
        "street1": { "type": "string" },
        "street2": { "type": "string" }
    }
}
```

## Features

- treat `Option` as optional fields
- as well as treat fields with `default values` as optional
- support `value classes`
- any `Traversable` will be treated as `array`
- has `TypeRegistry` which will help to gather all schemas used in project (might help building REST documentation, Swagger, RAML, etc)