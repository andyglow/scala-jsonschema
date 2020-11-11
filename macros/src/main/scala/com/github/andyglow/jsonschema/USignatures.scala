package com.github.andyglow.jsonschema


private[jsonschema] trait USignatures { this: UContext =>
  import c.universe._


  def signature(tpe: Type): String = {

    def compute(tpe: Type): String = {

      tpe match {
        case tpe: SingleType                                => tpe.typeSymbol.fullName
        case TypeRef(_, name, Nil)                          => name.fullName
        case ExistentialType(_, TypeRef(_, name, Nil))      => name.fullName
        case TypeRef(_, name, typeargs)                     => name.fullName + s"[${typeargs map compute mkString ","}]"
        case ExistentialType(_, TypeRef(_, name, typeargs)) => name.fullName + s"[${typeargs map compute mkString ","}]"
        case ConstantType(x)                                => x.value.toString
      }
    }

    compute(tpe)
  }
}
