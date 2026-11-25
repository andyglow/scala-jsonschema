package json.schema

/** A type class instance offered by an import (`import json.Json.auto._`) at a lower priority than every instance defined the usual way, as circe's and pureconfig's `Exported`.
  *
  * `Json.auto.derived[T]` is an `Exported[Predef[T]]`, not a `Predef[T]`: it can't win a search for a `Predef` by itself, only through the lowest-priority bridge `Predef.importedPredef`. So a
  * `Predef` or `Schema` in a companion, in a library or in local scope always wins over auto (issue #542).
  */
final case class Exported[+T](instance: T) extends AnyVal
