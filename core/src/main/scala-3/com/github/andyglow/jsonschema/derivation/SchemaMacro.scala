package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.jsonschema.TypeSignature
import json.Schema
import json.schema.{Exported, Predef}

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

  /** The schema of `json.schema.derived.DerivedSchema[T]` (module `derived`, which wraps it). */
  def derivedSchemaImpl[T: Type](using Quotes): Expr[Schema[T]] =
    Derivation.derive[T](Mode.DerivedSchema)

  /** @param allowed `auto.derived`'s evidence, unused here: its resolution is the point (see [[AutoGuard.Allowed]]) */
  def autoImpl[T: Type](@annotation.unused allowed: Expr[AutoGuard.Allowed])(using Quotes): Expr[Exported[Predef[T]]] =
    Derivation.deriveAuto[T]

  def sigImpl[T: Type](using Quotes): Expr[TypeSignature[T]] =
    Derivation.signature[T]
}

/** What the expansion was asked for. Decides the root's implicit search (see [[Resolve]]). */
enum Mode {

  /** `Json.schema[T]`: the root looks for an implicit `Schema[T]` / `Predef[T]` first, like every nested type. Semi-auto (issue #542): a nested case class or sealed family without an instance is
    * derived with a warning (an error under `Flag.SemiAuto`), unless `Json.auto` is imported.
    */
  case Schema

  /** `Json.objectSchema[T](...)`: the root must be a (non-value) case class; it is never looked up. Semi-auto, like [[Schema]]. */
  case ObjectSchema

  /** `derives json.Schema` / `Schema.derived[T]`: the root is always derived (it would find its own given). Semi-auto, like [[Schema]]. */
  case Derived

  /** `json.schema.derived.DerivedSchema[T]` (module `derived`): the root is always derived (so the type class works as a context bound without finding the type's own implicit `Schema`), and, unlike
    * [[Derived]], it stays inline, not a definition (Scala 2 parity: `noImplicitSearch = true` for the root, nothing else). Semi-auto, like [[Schema]], without the warning (it always runs inside an
    * implicit search).
    */
  case DerivedSchema

  /** `Json.auto.derived[T]` outside a derivation: deep, every nested type without an instance is derived in place. A user-provided implicit `Schema` still becomes a definition, a `Predef` is inlined.
    */
  case Auto
}
