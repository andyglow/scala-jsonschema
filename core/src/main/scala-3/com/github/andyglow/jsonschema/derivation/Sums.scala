package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema

/** Sum types: sealed traits / abstract classes with at least one non-singleton member, `Either` (Scala 2's `USumTypes`). STREAM 2.
  *
  * Contract (profiledesign facts 2-4; `notes/reports/skeleton-ir.md`): construction builds a [[Sum]] whose members are in declaration order (`Symbol.children`), NOT flattened and NOT merged. A nested
  * sealed family is a member of kind [[MemberKind.NestedFamily]] whose schema is its own [[Sum]]. Flattening nested families and merging case objects into one enum (Scala 2 output) happen in an IR ->
  * IR lowering pass or in emit, never in construction. Every member keeps its simple name, its signature, its own texts, `@discriminatorKey` and `@definition`, and singletons keep a reference to
  * their value.
  *
  * TODO(stream 2): [[deriveSum]], the lowering, [[Sum.emitRaw]]; generic members via `baseType` substitution (TRIAGE-B: by name, not by position); `@discriminator` field check; `@definition`;
  * `Either` as kind [[SumKind.Either]] (output pinned: oneOf of `object{value}`).
  */
trait Sums { this: Derivation =>
  import q.reflect.*

  enum SumKind {
    case Sealed, Either
  }

  enum MemberKind {
    case CaseClass, CaseObject, ValueClass, EnumClassCase, EnumSingletonCase, NestedFamily
  }

  /** @param name
    *   simple name (`Dog`)
    * @param sig
    *   full signature (`pkg.Animal.Dog`)
    * @param singleton
    *   the value of a singleton member (case object, enum singleton case), for ToValue / profiles
    * @param texts
    *   the member's own `@title` / `@description` / scaladoc, ALWAYS recorded (Scala 2 drops them for case objects unless EnumsAsOneOf: do that in emit)
    * @param definition
    *   `@definition` / `@definition("name")`
    */
  final case class SumMember(
    name: String,
    sig: String,
    kind: MemberKind,
    singleton: Option[Term],
    texts: Extra,
    discriminatorKey: Option[String],
    definition: Option[Arg[String]],
    schema: Node
  )

  /** @param discriminator
    *   `@discriminator` on the family, user literal vs annotation default kept ([[Arg]])
    * @param enumsAsOneOf
    *   the `Flag.EnumsAsOneOf` in scope, recorded; consult it in emit
    */
  final case class Sum(
    tpe: TypeRepr,
    kind: SumKind,
    members: List[SumMember],
    discriminator: Option[TypeAnnotations.Discriminator],
    typeHint: Option[TypeRepr],
    enumsAsOneOf: Boolean,
    extra: Extra = Extra()
  ) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = members.map(_.schema)
    def mapChildren(f: Node => Node): Node = copy(members = members.map(m => m.copy(schema = f(m.schema))))
    def emitRaw: Expr[Schema[?]]           = notYet(tpe, "sum types (stream 2)")
  }

  /** TODO(stream 2). */
  def deriveSum(tpe: TypeRepr, ctx: Ctx): Option[Node] = None

  /** A placeholder for families that aren't ported yet. */
  def notYet(tpe: TypeRepr, what: String): Nothing =
    report.errorAndAbort(s"schema for ${tpe.show} is not supported yet on Scala 3: $what")
}
