package json

import com.github.andyglow.jsonschema._
import com.github.andyglow.jsonschema.derivation.{AutoGuard, SchemaMacro}
import json.Schema.`object`
import json.schema.{Predef, Version}

/** The Scala 3 facade, source-compatible with the Scala 2 one (`api/src/main/scala-2/json/Json.scala`). The macro implementation lives in `core` (`com.github.andyglow.jsonschema.derivation`). */
object Json {

  inline def schema[T]: Schema[T] = ${ SchemaMacro.schemaImpl[T] }

  inline def objectSchema[T](inline decorations: (String, String)*): `object`[T] = ${ SchemaMacro.objectSchemaImpl[T]('decorations) }

  inline def sig[T]: TypeSignature[T] = ${ SchemaMacro.sigImpl[T] }

  /** Automatic derivation: `import json.Json.auto.given` (or Scala 2's `import json.Json.auto._`) makes every type without its own schema derivable as a `Predef`. Nested types follow the plain
    * derivation rules: a user-provided implicit `Schema` becomes a definition, a `Predef` is inlined, anything else is derived inline.
    *
    * The implicit `AutoGuard.Allowed` is always available; it keeps auto out of a derivation's own implicit searches (see `derivation.AutoGuard.Allowed`).
    */
  object auto {

    implicit inline def derived[T](implicit allowed: AutoGuard.Allowed): Predef[T] = ${ SchemaMacro.autoImpl[T]('allowed) }
  }

  def stringify[T, V <: Version: AsValueBuilder](schema: Schema[T], version: V): String =
    schema.stringify(version)
}
