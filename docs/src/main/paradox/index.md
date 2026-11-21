# Scala JSON Schema

Generate [JSON Schema](https://json-schema.org) from Scala types at compile time, the way JSON libraries derive
their codecs. Inspired by Coursera's [AutoSchema](https://github.com/coursera/autoschema), but built on macros
instead of runtime reflection. For Scala 2.12, 2.13 and 3.3 LTS.

- Case classes (optional `Option` fields, default values), value classes, generic and recursive types
- Sealed families, Scala 3 `enum`s, `scala.Enumeration` and Java enums
- Collections and maps, `java.time`, `UUID`, `URI`, ... out of the box
- Titles and descriptions from annotations, from scaladoc, or passed in explicitly
- Validations (`pattern`, `minimum`, `maxItems`, ...)
- Renders Draft 04, 06, 07, 2019-09 and 2020-12, as its own JSON AST or as play-json, circe, spray-json, json4s or ujson
- Support for refined, cats, enumeratum and joda-time

@@@ index

* [Getting Started](getting-started.md)
* [Definitions](definitions.md)
* [Types and Enums](types.md)
* [Descriptions](descriptions.md)
* [Validation](validation.md)
* [Rendering](rendering.md)
* [Modules](modules/index.md)
* [Migrating to 0.8.0](migration.md)
* [Scala 3](scala3.md)

@@@
