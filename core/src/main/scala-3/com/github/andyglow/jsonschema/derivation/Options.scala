package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema

/** `Option[X]`: an explicit [[Opt]] node in every position (root, collection element, map value, field), recognised before the sum families (profiledesign fact 1). Nesting is kept:
  * `Option[Option[X]]` is `Opt(Opt(X))`.
  *
  * Emitted as the `option` node, which a `json.schema.JsonSchemaProfile` shapes when the schema is rendered: the default (Neutral) as "X or null", Legacy as the 0.7 shapes (a field: X, not required;
  * anywhere else `oneOf(object{value: X}, enum["None"])`). A field is not required either way (see [[Products]]).
  */
trait Options { this: Derivation =>
  import q.reflect.*

  /** @param tpe `Option[X]` */
  final case class Opt(tpe: TypeRepr, inner: Node, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(inner)
    def mapChildren(f: Node => Node): Node = copy(inner = f(inner))
    def emitRaw: Expr[Schema[?]]           = inner.tpe.asType match {
      case '[x] =>
        val value = emit(inner).asExprOf[Schema[x]]
        '{ Schema.`option`[x]($value) }
    }
  }

  /** `Option[e]`. */
  def optionOf(e: TypeRepr): TypeRepr = TypeRepr.of[Option[Any]].typeSymbol.typeRef.appliedTo(e)

  def deriveOption(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    if (classSymbol(tpe) == TypeRepr.of[Option[Any]].typeSymbol) optionElement(tpe).map(e => Opt(tpe, resolve(e, ctx.descend(tpe))))
    else None
}
