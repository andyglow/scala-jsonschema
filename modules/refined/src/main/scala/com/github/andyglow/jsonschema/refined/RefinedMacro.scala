package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api.Refined

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class RefinedMacro(val c: blackbox.Context) extends Logic {
  import c.universe._

  def forTypeParams[A, B](implicit a: c.WeakTypeTag[A], b: c.WeakTypeTag[B]): c.Expr[json.schema.Predef[Refined[A, B]]] = {
    val t = typeOf[eu.timepit.refined.api.Refined[_, _]]
    val tt = appliedType(t.typeConstructor, List(a.tpe, b.tpe))
    val tree = forRefinedType(c.WeakTypeTag(tt)).tree

    c.Expr[json.schema.Predef[Refined[A, B]]](q"json.schema.Predef($tree)")
  }

  def forRefinedType[T](implicit t: c.WeakTypeTag[T]): c.Expr[json.Schema[T]] = {
    import c.universe._
    val tt = t.tpe.dealias

    dbg("\n---------------\n"+showRaw(tt)+"\n---------------")

    val tree = gen(tt)

    dbg(showCode(tree))

    c.Expr[json.Schema[T]](
      q"""
        import json.Schema._
        import `string`._
        import Format._
        import json.Validation._

        $tree.asInstanceOf[json.Schema[$tt]]
      """)
  }
}