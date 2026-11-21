package com.github.andyglow.jsonschema

private[jsonschema] trait USignatures { this: UContext =>
  import c.universe._

  def signature(tpe: Type): String = {

    def compute(tpe: Type): String = {

      tpe match {
        case AnnotatedType(_, underlying)                   => compute(underlying)
        case RefinedType(parents, decls) if decls.isEmpty   => parents map compute mkString " with "
        case tpe: SingleType                                => tpe.typeSymbol.fullName
        case ThisType(sym)                                  => sym.fullName
        case TypeRef(_, name, Nil)                          => name.fullName
        case ExistentialType(_, TypeRef(_, name, Nil))      => name.fullName
        case TypeRef(_, name, typeargs)                     => name.fullName + s"[${typeargs map compute mkString ","}]"
        case ExistentialType(_, TypeRef(_, name, typeargs)) =>
          name.fullName + s"[${typeargs map compute mkString ","}]"
        case ConstantType(x) => x.value.toString
        case other           =>
          c.abort(
            c.enclosingPosition,
            s"schema for $other is not supported: a definition can't be named after this type (a structural or other unnamed type). Use a named type, e.g. a case class"
          )
      }
    }

    compute(tpe)
  }
}
