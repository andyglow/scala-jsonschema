package com.github.andyglow.jsonschema.refined

import scala.reflect.macros.blackbox

private[jsonschema] trait Logic extends Extractors with HasLog with AST with Math with HasContext {
  import refined._

  val c: blackbox.Context
  import c.universe._

  def gen(t: Type): Tree = {

    t match {
      case R(t, p) =>
        (t, p) match {
          case P(pp) => pp.norm.tree
          case _     => warn(s"Can't infer Predicate out of ${showRaw(p)}"); EmptyTree
        }
      case _ => warn(s"Can't infer Refined out of ${showRaw(t)}"); EmptyTree
    }
  }
}
