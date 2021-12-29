# Scala ↦ JsonSchema
Derives [json-schema](https://json-schema.org/) from your scala models.

## Overview
This library aims to play the role of the bridge between your models defined in scala
and the rest of the non-scala ecosystem keeping scala models a single source of truth.    

## Features
- Spec Versions Supported 
    - <input type="checkbox" checked="true" disabled="true"> `draft-04`
    - <input type="checkbox" checked="true" disabled="true"> `draft-06`
    - <input type="checkbox" checked="true" disabled="true"> `draft-07`
    - <input type="checkbox" checked="true" disabled="true"> `draft-09`
- <input type="checkbox" checked="true" disabled="true"> Support for scala type system
    - <input type="checkbox" checked="true" disabled="true"> `case classes` (product types)
        - <input type="checkbox" checked="true" disabled="true"> including `default values`
    - <input type="checkbox" checked="true" disabled="true"> `sealed trait hierarchies` (sum types)
    - <input type="checkbox" checked="true" disabled="true"> `value classes`
    - <input type="checkbox" checked="true" disabled="true"> `generic types`
    - <input type="checkbox" checked="true" disabled="true"> `recursive types`
- Extensions
    - <input type="checkbox" checked="true" disabled="true"> [joda-time](https://github.com/JodaOrg/joda-time)
    - <input type="checkbox" checked="true" disabled="true"> [cats](https://github.com/typelevel/cats)
    - <input type="checkbox" checked="true" disabled="true"> [refined](https://github.com/fthomas/refined)
    - <input type="checkbox" checked="true" disabled="true"> [enumeratum](https://github.com/lloydmeta/enumeratum)
- Integrations
    - <input type="checkbox" checked="true" disabled="true"> [play-json](https://github.com/playframework/play-json)
    - <input type="checkbox" checked="true" disabled="true"> [spray-json](https://github.com/spray/spray-json)
    - <input type="checkbox" checked="true" disabled="true"> [json4s](https://github.com/json4s/json4s)
    - <input type="checkbox" checked="true" disabled="true"> [circe](https://github.com/circe/circe)
    - <input type="checkbox" checked="true" disabled="true"> [ujson](https://www.lihaoyi.com/post/uJsonfastflexibleandintuitiveJSONforScala.html)
    
## Example

Let's take a look at the example code 

model
:   @@snip [model.md](../snippets/index-snippet.scala) { #setup }

json
:   @@snip [model.md](../snippets/index-snippet.scala) { #result }