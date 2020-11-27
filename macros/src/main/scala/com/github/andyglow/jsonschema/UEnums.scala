package com.github.andyglow.jsonschema


private[jsonschema] trait UEnums { this: UContext with UCommons =>
  import c.universe._

  private val ScalaEnumTpe = typeOf[scala.Enumeration]

  class EnumExtractor {

    private def fromSumType(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {
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

    private def fromScalaEnumeration(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {

      // Given that `tpe` can refer to different aspects of enum
      // - WeekDay.type
      // - WeekDay.Value
      // - WeekDay.AlasedTpe. eg. `type AliasedTpe = Value`
      // we need to determine root enum object type
      val objTpe = tpe.dealias.map {
        case TypeRef((x, _, _)) => x // schema[WeekDay.T] | schema[WeekDay.Value]
        case x                  => x // schema[WeekDay.type]
      }

      // did we find scala enum object?
      val isScalaEnum = objTpe.baseClasses exists { tpeSym =>
        tpeSym.isClass && tpeSym.asClass.toType =:= ScalaEnumTpe
      }

      // is this enum definition member really an enum instance?
      def getEnumName(sym: Symbol): Option[String] = {
        def isMember: Boolean =
          sym.isDefined &&
          sym.isTerm &&
          sym.isMethod &&
          !sym.isConstructor &&
          sym.asMethod.isGetter &&
          sym.asMethod.returnType.baseClasses.filter(_.isClass).exists { sup =>
            sup.asClass.asType.name.decodedName.toString == "Value"
          }

        Some.when(isMember) {
          sym.asTerm.name.decodedName.toString.trim
        }
      }

      Some.when(isScalaEnum) {
        val valueNames = objTpe.decls.flatMap(getEnumName).toList.distinct
        val valueTrees = valueNames map { moduleName => q"${N.internal.json}.Value.str($moduleName)" }
        U.Enum(tpe, valueTrees, Some(valueNames))
      }
    }

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {
      fromScalaEnumeration(tpe) orElse fromSumType(tpe)
    }
  }

  val Enum = new EnumExtractor
}
