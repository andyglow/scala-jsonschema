package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema

/** Case value classes (`case class Id(v: String) extends AnyVal`) -> `value-class`, rendered as the wrapped type (Scala 2's `UValueTypes`). The wrapper's field name and signature are kept in the IR
  * (profiledesign fact 5). Non-case value classes are not supported (Scala 2 parity).
  *
  * Owner: skeleton + stream 1. The inner type is `memberType`-substituted, so a generic value class `Box[T](v: T)` gets its type argument (TRIAGE-B).
  */
trait ValueClasses { this: Derivation =>
  import q.reflect.*

  /** @param fieldName the wrapped field (`v` in `Id(v: String)`) */
  final case class ValueClass(tpe: TypeRepr, innerTpe: TypeRepr, fieldName: String, inner: Node, extra: Extra = Extra()) extends Node {
    def sig: String                        = signature(tpe, dealiased = true)
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(inner)
    def mapChildren(f: Node => Node): Node = copy(inner = f(inner))
    def emitRaw: Expr[Schema[?]]           = (tpe.asType, innerTpe.asType) match {
      case ('[o], '[i]) =>
        val is = emit(inner).asExprOf[Schema[i]]
        '{ Schema.`value-class`[o, i]($is) }
    }
  }

  def deriveValueClass(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    if (isValueClassSymbol(tpe) && classSymbol(tpe).flags.is(Flags.Case)) {
      params(tpe) match {
        case p :: Nil => Some(ValueClass(tpe, p.declared, p.name, resolve(p.declared, ctx.descend(tpe))))
        case _        => None
      }
    } else None
}
