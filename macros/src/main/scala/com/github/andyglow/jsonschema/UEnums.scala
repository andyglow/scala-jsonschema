package com.github.andyglow.jsonschema


private[jsonschema] trait UEnums { this: UContext with UCommons =>
  import c.universe._

  class EnumExtractor {
    // TODO: add support for enumeratum/scala enumerations

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {
      Option.whenever (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
        val instances = tpe.typeSymbol.asClass.knownDirectSubclasses
        val toValueTree = c.inferImplicitValue(
          appliedType(T.toValue, tpe),
          silent = true,
          withMacrosDisabled = true)

        Option.whenever (instances forall { i => val c = i.asClass; c.isModuleClass}) {
          if (toValueTree.nonEmpty) {
            val valueTrees = instances.toSeq collect {
              case i: ClassSymbol =>
                val caseObj = i.owner.asClass.toType.decls.find { d =>
                  d.name == i.name.toTermName
                } getOrElse NoSymbol

                q"$toValueTree($caseObj)"
            }
            Some(U.Enum(tpe, valueTrees, None))
          } else {
            val valueNames = instances.toSeq map { module => module.name.decodedName.toString }
            val valueTrees = valueNames map { moduleName => q"${N.internal.json}.Value.str($moduleName)" }
            Some(U.Enum(tpe, valueTrees, Some(valueNames)))
          }
        }
      }
    }
  }

  val Enum = new EnumExtractor
}
