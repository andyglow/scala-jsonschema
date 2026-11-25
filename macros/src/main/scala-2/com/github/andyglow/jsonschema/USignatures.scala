package com.github.andyglow.jsonschema

import scala.reflect.NameTransformer

private[jsonschema] trait USignatures { this: UContext with UCommons =>
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
            s"schema for ${showType(other)} is not supported: a definition can't be named after this type (a structural or other unnamed type). Use a named type, e.g. a case class"
          )
      }
    }

    compute(tpe)
  }

  /** `tpe` is, or has as a type argument, a type parameter or an abstract type member: its signature would name the parameter (`pkg.Owner.T`), the same for every instantiation (#297). Existentially
    * bound types (`Foo[_]`) don't count. Scala 3: `Implicits.mentionsAbstractType`.
    */
  def mentionsAbstractType(tpe: Type): Boolean = tpe.dealias match {
    // `isAbstract` is also true for a trait or an abstract class: only a type that isn't a class (a type parameter or an abstract type member) counts
    case TypeRef(_, sym, args) => (sym.isType && !sym.isClass && sym.asType.isAbstract && !sym.asType.isExistential) || args.exists(mentionsAbstractType)
    // `Two[A, _]`: look inside, where the guard above skips the existential's own `_$1`
    case ExistentialType(_, u)   => mentionsAbstractType(u)
    case RefinedType(parents, _) => parents.exists(mentionsAbstractType)
    case AnnotatedType(_, u)     => mentionsAbstractType(u)
    case _                       => false
  }
}
