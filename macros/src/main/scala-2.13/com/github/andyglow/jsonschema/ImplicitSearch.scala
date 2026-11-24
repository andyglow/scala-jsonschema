package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

// Scala 2.13: an implicit search that leaves out some symbols. `c.inferImplicitValueIgnoring` is new in 2.13.17. With an older compiler
// (the macro runs on the user's scala-reflect) the call fails to link: `None`, and the caller searches the plain way.
private[jsonschema] object ImplicitSearch {

  @volatile private var ignoringAvailable = true

  def inferIgnoring(c: blackbox.Context)(pt: c.universe.Type, ignored: Seq[c.universe.Symbol]): Option[c.universe.Tree] =
    if (!ignoringAvailable) None
    else
      try Some(c.inferImplicitValueIgnoring(pt)(ignored: _*))
      catch {
        case _: IncompatibleClassChangeError =>
          ignoringAvailable = false
          None
      }
}
