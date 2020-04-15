package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox


object TypeSignatureMacro {

  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[TypeSignature[T]] = {
    import c.universe._

    c.Expr[TypeSignature[T]] { typeSig(c)(weakTypeOf[T]) }
  }

  private[jsonschema] def typeSig(c: blackbox.Context)(tpe: c.universe.Type): c.Tree = {
    import c.universe._

    def compute(tpe: c.universe.Type): String = {
//      c.info(c.enclosingPosition, showRaw(tpe), force = true)

      tpe match {
        case tpe: SingleType                                => tpe.typeSymbol.fullName
        case TypeRef(_, name, Nil)                          => name.fullName
        case ExistentialType(_, TypeRef(_, name, Nil))      => name.fullName
        case TypeRef(_, name, typeargs)                     => name.fullName + s"[${typeargs map compute mkString ","}]"
        case ExistentialType(_, TypeRef(_, name, typeargs)) => name.fullName + s"[${typeargs map compute mkString ","}]"
        case ConstantType(x)                                => x.value.toString
      }
    }

    val name = compute(tpe)

    q"_root_.com.github.andyglow.jsonschema.TypeSignature[$tpe]($name)"
  }
}
