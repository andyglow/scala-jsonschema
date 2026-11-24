# Descriptions

There are three sources of titles and descriptions: annotations, scaladoc, and decorations passed to
`Json.objectSchema`.

```scala mdoc:compile-only
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

## Scaladoc

The doc comment right before a class (annotations, modifiers and ordinary comments may sit in between) gives its
description, and its `@param` tags the field descriptions. A class without a doc comment gets none. A `@param`
for a field that doesn't exist is a warning. Where the comment comes from:

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

| annotation                                    | on                      | effect                                                    |
|-----------------------------------------------|-------------------------|-----------------------------------------------------------|
| `@title("...")`                               | type, enum value        | `title`                                                   |
| `@description("...")`                         | type, field, enum value | `description`                                             |
| `@readOnly`                                   | field                   | `"readOnly": true`                                        |
| `@writeOnly`                                  | field                   | `"writeOnly": true`                                       |
| `@deprecated` (Scala's, or Java's `@Deprecated`) | field, type          | `"deprecated": true` (draft 2019-09 and 2020-12; not under the Legacy profile) |
| `@discriminator(field = "type", phantom = true)` | sealed root          | adds a discriminator field to each `oneOf` member         |
| `@discriminatorKey("key")`                    | sealed member           | the member's discriminator value (default: its full name) |
| `@definition` / `@definition("name")`         | sealed member           | makes it a definition                                     |
| `@typeHint[T]`                                | sealed root, member     | the JSON type of enum values that come from a `ToValue`   |

On Scala 2, a field annotation can also target the underlying field: `@(description @field)("...")`.
