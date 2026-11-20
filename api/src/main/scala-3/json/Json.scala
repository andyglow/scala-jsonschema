package json

import com.github.andyglow.jsonschema._
import json.Schema.`object`
import json.schema.{Predef, Version}

// Placeholder so that `api` (and everything depending on it) cross-builds to Scala 3.
// The Scala 3 derivation (inline/quotes) replaces these `compiletime.error` bodies.
object Json {

  inline def schema[T]: Schema[T] = compiletime.error("Json.schema is not implemented on Scala 3 yet")

  inline def objectSchema[T](inline decorations: (String, String)*): `object`[T] = compiletime.error("Json.objectSchema is not implemented on Scala 3 yet")

  inline def sig[T]: TypeSignature[T] = compiletime.error("Json.sig is not implemented on Scala 3 yet")

  object auto {

    implicit inline def derived[T]: Predef[T] = compiletime.error("Json.auto.derived is not implemented on Scala 3 yet")
  }

  def stringify[T, V <: Version: AsValueBuilder](schema: Schema[T], version: V): String =
    schema.stringify(version)
}
