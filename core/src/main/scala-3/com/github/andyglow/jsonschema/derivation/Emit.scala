package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema

/** Emission: IR -> `Expr[Schema[?]]`. A separate pass from construction: every node emits itself ([[Nodes.Node.emitRaw]]), and [[emit]] adds the node's [[Nodes.Extra]] the way Scala 2's
  * `SchemaType.tree` does: `.withValidation(..)`, `.withTitle(..)`, `.withDescription(..)`, `.withDiscriminationKey(..)`.
  *
  * Gotcha (Scala 3 quotes): don't call `Expr(..)`, `asExprOf` or `emit(..)` inside a `'{ ... }`; hoist them into vals first, or the two `Quotes` in scope are ambiguous.
  */
trait Emit { this: Derivation =>

  def emit(n: Node): Expr[Schema[?]] = withExtra(n.emitRaw, n.extra)

  def withExtra(e: Expr[Schema[?]], x: Extra): Expr[Schema[?]] = {
    val e0 = x.validations.foldLeft(e)((acc, v) => v(acc))
    val e1 = x.title.fold(e0) { t =>
      val te = Expr(t)
      '{ $e0.withTitle($te) }
    }
    val e2 = x.description.fold(e1) { d =>
      val de = Expr(d)
      '{ $e1.withDescription($de) }
    }
    x.discriminationKey.fold(e2) { k =>
      val ke = Expr(k)
      '{ $e2.withDiscriminationKey($ke) }
    }
  }
}
