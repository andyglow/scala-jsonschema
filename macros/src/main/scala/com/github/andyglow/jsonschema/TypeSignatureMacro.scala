package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

object TypeSignatureMacro {

  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[TypeSignature[T]] = {
    import c.universe._

    c.Expr[TypeSignature[T]] { typeSig(c)(weakTypeOf[T]) }
  }

  private[jsonschema] def typeSig(c: blackbox.Context)(tpe: c.universe.Type): c.Tree = {
    import c.universe._

    val localName = tpe.typeSymbol.fullName

    val name = if (tpe.typeArgs.isEmpty) localName else {
      localName + s"[${tpe.typeArgs map { _.typeSymbol.fullName } mkString ","}]"
    }

    q"_root_.com.github.andyglow.jsonschema.TypeSignature[$tpe]($name)"
  }
}
