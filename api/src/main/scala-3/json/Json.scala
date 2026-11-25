package json

import com.github.andyglow.jsonschema._
import com.github.andyglow.jsonschema.derivation.{AutoGuard, SchemaMacro}
import json.Schema.`object`
import com.github.andyglow.jsonschema.internal.AutoEnabled
import json.schema.{Exported, JsonSchemaProfile, Predef, Version}

/** The Scala 3 facade, source-compatible with the Scala 2 one (`api/src/main/scala-2/json/Json.scala`). The macro implementation lives in `core` (`com.github.andyglow.jsonschema.derivation`). */
object Json {

  inline def schema[T]: Schema[T] = ${ SchemaMacro.schemaImpl[T] }

  inline def objectSchema[T](inline decorations: (String, String)*): `object`[T] = ${ SchemaMacro.objectSchemaImpl[T]('decorations) }

  inline def sig[T]: TypeSignature[T] = ${ SchemaMacro.sigImpl[T] }

  /** Automatic (deep) derivation, as circe's and pureconfig's `auto` (issue #542): `import json.Json.auto.given` (or Scala 2's `import json.Json.auto._`).
    *
    * Without it, `Json.schema` and the other entry points derive the type asked for (and the cases of a sealed family); a nested case class or sealed family needs an implicit `Schema` or `Predef` (in
    * 0.10 it is still derived, with a deprecation warning; `Flag.SemiAuto` makes that an error now). With it, every nested type without an instance is derived in place, in the same expansion: a
    * user-provided implicit `Schema` still becomes a definition, a `Predef` is inlined.
    *
    * `derived[T]` is an `Exported[Predef[T]]`, reached only through the lowest-priority bridge `Predef.importedPredef`, so every instance defined the usual way wins over it. Called outside a
    * derivation (`implicitly[Predef[T]]`), it derives `T` deeply; inside a derivation's implicit search it is a placeholder that the derivation fills (see `derivation.AutoGuard`). The implicit
    * `AutoGuard.Allowed` is always available.
    */
  object auto {

    implicit inline def derived[T](implicit allowed: AutoGuard.Allowed): Exported[Predef[T]] = ${ SchemaMacro.autoImpl[T]('allowed) }

    /** The marker every derivation with `Json.auto` in scope refers to, so the import counts as used (also when no tree found through it ends up in the output). */
    implicit val jsonSchemaAutoEnabled: AutoEnabled = AutoEnabled.instance
  }

  /** `schema` rendered for `version`, as a string. Its `Option`s are shaped by the implicit [[json.schema.JsonSchemaProfile]] in scope, by default `JsonSchemaProfile.Neutral`. */
  def stringify[T, V <: Version](schema: Schema[T], version: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): String =
    schema.stringify(version)
}
