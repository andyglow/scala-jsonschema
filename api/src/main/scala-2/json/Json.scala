package json

import com.github.andyglow.jsonschema._
import json.Schema.`object`
import com.github.andyglow.jsonschema.internal.AutoEnabled
import json.schema.{Exported, JsonSchemaProfile, Predef, Version}

import scala.language.experimental.macros

object Json {

  def schema[T]: Schema[T] = macro Macroses.deriveSchema[T]

  def objectSchema[T](decorations: (String, String)*): `object`[T] =
    macro Macroses.deriveObjectSchema[T]

  def sig[T]: TypeSignature[T] = macro Macroses.deriveSignature[T]

  /** Automatic (deep) derivation: `import json.Json.auto._`. Without it, `Json.schema[T]` derives `T` itself (and the cases of a sealed `T`), and every nested case class or sealed family needs an
    * implicit `json.Schema` (a definition + `$ref`) or `json.schema.Predef` (inlined) of its own (semi-auto, issue #542). With it, those are derived in place, in the same expansion.
    *
    * `derived` is an [[json.schema.Exported]] `Predef`, as circe's and pureconfig's auto: it is reached only through the lowest-priority bridge `Predef.importedPredef`, so every `Predef` or `Schema`
    * defined the usual way (built-ins, companions, locals, imports) wins over it. Called outside a derivation it derives `T` deeply; inside a derivation's implicit search it is a placeholder the
    * derivation fills in.
    */
  object auto {

    implicit def derived[T]: Exported[Predef[T]] = macro Macroses.deriveAuto[T]

    /** INTERNAL API: marks the import as used (every auto-mode expansion refers to it). */
    implicit val jsonSchemaAutoEnabled: AutoEnabled = AutoEnabled.instance
  }

  /** `schema` rendered for `version`, as a string. Its `Option`s are shaped by the implicit [[json.schema.JsonSchemaProfile]] in scope, by default `JsonSchemaProfile.Neutral`. */
  def stringify[T, V <: Version](schema: Schema[T], version: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): String =
    schema.stringify(version)
}
