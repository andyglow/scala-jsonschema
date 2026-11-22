package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.json.Value
import json.Schema

/** `Option[X]`: an explicit [[Opt]] node in every position (root, collection element, map value, field), recognised before the sum families (profiledesign fact 1). Nesting is kept:
  * `Option[Option[X]]` is `Opt(Opt(X))`.
  *
  * v1 rendering (parity with Scala 2, which derives `Option` as the sealed family `Some | None`):
  *   - a field: the element's schema, not required (see [[Products]]: the field unwraps one `Opt`);
  *   - anywhere else: `oneOf(object{value: X}, enum["None"])`. TRIAGE-C: PENDING, the owner decides after "optenc" (probably "X or null"). Only [[Opt.emitRaw]] changes then.
  */
trait Options { this: Derivation =>
  import q.reflect.*

  final case class Opt(tpe: TypeRepr, inner: Node, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(inner)
    def mapChildren(f: Node => Node): Node = copy(inner = f(inner))
    def emitRaw: Expr[Schema[?]]           = (tpe.asType, inner.tpe.asType) match {
      case ('[t], '[x]) =>
        val value = emit(inner).asExprOf[Schema[x]]
        '{
          Schema.`oneof`[t](
            Set[Schema[?]](
              Schema.`object`.fromList[Some[x]](List(Schema.`object`.Field[x]("value", $value, true))),
              Schema.`enum`.of[None.type](Schema.`string`, Value.str("None"))
            ),
            None
          )
        }
    }
  }

  def deriveOption(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    if (classSymbol(tpe) == TypeRepr.of[Option[Any]].typeSymbol) optionElement(tpe).map(e => Opt(tpe, resolve(e, ctx.descend(tpe))))
    else None
}
