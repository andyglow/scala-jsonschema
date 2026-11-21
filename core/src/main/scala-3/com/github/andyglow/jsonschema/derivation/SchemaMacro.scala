package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.jsonschema.TypeSignature
import json.Schema
import json.schema.Predef

/** Entry points of the Scala 3 derivation (the `${ ... }` targets).
  *
  * INTERNAL API: users call `json.Json.schema` / `objectSchema` / `sig` / `auto.derived` (module `api`) or `derives json.Schema`. The implementation lives in `core` because `Schema.derived` has to be
  * a member of `object json.Schema`, which needs the macro implementation compiled with (or before) it.
  */
object SchemaMacro {

  def schemaImpl[T: Type](using Quotes): Expr[Schema[T]] =
    Derivation.derive[T](Mode.Schema)

  def objectSchemaImpl[T: Type](decorations: Expr[Seq[(String, String)]])(using Quotes): Expr[Schema.`object`[T]] =
    Derivation.deriveObject[T](decorations)

  def derivedImpl[T: Type](using Quotes): Expr[Schema[T]] =
    Derivation.derive[T](Mode.Derived)

  def autoImpl[T: Type](using Quotes): Expr[Predef[T]] =
    Derivation.deriveAuto[T]

  def sigImpl[T: Type](using Quotes): Expr[TypeSignature[T]] =
    Derivation.signature[T]
}

/** What the expansion was asked for. Decides the root's implicit search (see [[Resolve]]). */
enum Mode {

  /** `Json.schema[T]`: the root looks for an implicit `Schema[T]` / `Predef[T]` first, like every nested type. */
  case Schema

  /** `Json.objectSchema[T](...)`: the root must be a (non-value) case class; it is never looked up. */
  case ObjectSchema

  /** `derives json.Schema` / `Schema.derived[T]`: the root is always derived (it would find its own given). */
  case Derived

  /** `Json.auto.derived[T]`: like [[Schema]] (owner decision 2026-11-20: nested types follow the plain derivation rules); auto's own given never counts. */
  case Auto
}
