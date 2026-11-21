package com.github.andyglow.jsonschema

import scala.reflect.NameTransformer

private[jsonschema] trait USignatures { this: UContext =>
  import c.universe._

  /** The full name with every segment decoded (`odd name`, `+`), as Scala 3's `fullName` gives it; Scala 2's is encoded (`odd$u0020name`, `$plus`). Encoding happens only when rendering (see
    * DefinitionNames in core).
    */
  private def decodedFullName(sym: Symbol): String = sym.fullName.split('.').map(NameTransformer.decode).mkString(".")

  def signature(tpe: Type): String = {

    def compute(tpe: Type): String = {

      tpe match {
        case AnnotatedType(_, underlying)                   => compute(underlying)
        case RefinedType(parents, decls) if decls.isEmpty   => parents map compute mkString " with "
        case tpe: SingleType                                => decodedFullName(tpe.typeSymbol)
        case ThisType(sym)                                  => decodedFullName(sym)
        case TypeRef(_, name, Nil)                          => decodedFullName(name)
        case ExistentialType(_, TypeRef(_, name, Nil))      => decodedFullName(name)
        case TypeRef(_, name, typeargs)                     => decodedFullName(name) + s"[${typeargs map compute mkString ","}]"
        case ExistentialType(_, TypeRef(_, name, typeargs)) =>
          decodedFullName(name) + s"[${typeargs map compute mkString ","}]"
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
