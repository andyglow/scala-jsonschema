# Json

The library uses its own Json model _com.github.andyglow.json.Value_ to represent Json Schema as JSON document.
But project contains additionally several modules which could connect it with library of your choice.

Currently supported:
- Play Json
- Spray Json
- Circe
- Json4s
- uJson

## Play Json
Example:
```scala
import com.github.andyglow.jsonschema.AsPlay._
import json.schema.Version._

import play.api.libs.json._

case class Foo(name: String)

val fooSchema: JsValue = Json.schema[Foo].asPlay(Draft04())
``` 

## Spray Json
Example:
```scala
import com.github.andyglow.jsonschema.AsSpray._
import json.schema.Version._

import spray.json._

case class Foo(name: String)

val fooSchema: JsValue = Json.schema[Foo].asSpray(Draft04())
``` 

## Circe
Example:
```scala
import com.github.andyglow.jsonschema.AsCirce._
import json.schema.Version._
import io.circe._

case class Foo(name: String)

val fooSchema: Json = Json.schema[Foo].asCirce(Draft04())
``` 

## Json4s
Example:
```scala
import com.github.andyglow.jsonschema.AsJson4s._
import json.schema.Version._
import org.json4s.JsonAST._

case class Foo(name: String)

val fooSchema: JValue = Json.schema[Foo].asJson4s(Draft04())
``` 

## uJson
```scala
import com.github.andyglow.jsonschema.AsU._
import json.schema.Version._

case class Foo(name: String)

val fooSchema: ujson.Value = Json.schema[Foo].asU(Draft04())
``` 