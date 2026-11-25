package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

// Scala 2.12: the macro API has no implicit search that leaves out symbols (2.13.17 adds one), and the check that reports a
// self-resolving implicit isn't part of 2.12's -Xlint (it's -Ywarn-self-implicit). `None`: the caller searches the plain way and
// declines a self reference afterwards.
private[jsonschema] object ImplicitSearch {

  def inferIgnoring(c: blackbox.Context)(pt: c.universe.Type, ignored: Seq[c.universe.Symbol]): Option[c.universe.Tree] = {
    val _ = (c, pt, ignored)
    None
  }
}
