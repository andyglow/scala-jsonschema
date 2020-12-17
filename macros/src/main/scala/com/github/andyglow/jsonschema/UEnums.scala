package com.github.andyglow.jsonschema


private[jsonschema] trait UEnums { this: UContext with UCommons =>
  import c.universe._

  private val ScalaEnumTpe = typeOf[scala.Enumeration]

  // ISSUE: https://github.com/andyglow/scala-jsonschema/issues/106
  // Used to extract case objects from sealed trait hierarchy
  case class SumTypeEnumCollector(
    tpe: Type,
    trees: Seq[(Tree, Option[String])] = Seq.empty) {

    private val toValueTree = c.inferImplicitValue(
      appliedType(T.toValue, tpe),
      silent = true,
      withMacrosDisabled = true)

    private lazy val instances = resolveSumTypeRecursively(
      tpe,
      include = isCaseObject,
      otherwise = _ => NoType)


    def includesOnlySupportedSymbols(): Boolean = !instances.contains(NoType)

    def resolved(): Option[SumTypeEnumCollector] = {
      instances.find(_ == NoType) foreach { _ =>
        abort(
          s"Only Scala case objects are supported for Sum Type leaves\nPlease consider using of " +
            s"them for Sum Types with base '$tpe' or provide a custom implicitly accessible json.Schema for the Sum Type.")
      }

      withInstancesReplaced(instances.map(_.typeSymbol).toSet)
    }

    def withInstancesReplaced(instances: Set[Symbol]): Option[SumTypeEnumCollector] = {
      Option.whenever (instances forall isCaseObject) {
        if (toValueTree.nonEmpty) {
          val valueTrees = instances.toSeq collect {
            case i: ClassSymbol =>
              val caseObj = i.owner.asClass.toType.decls.find { d =>
                d.name == i.name.toTermName
              } getOrElse NoSymbol

              q"$toValueTree($caseObj)"
          }
          copy(trees = valueTrees map { t => (t, None) })
        } else {
          val valueNames = instances.toSeq map { module => module.name.decodedName.toString }
          val valueTrees = valueNames map { moduleName => (q"${N.internal.json}.Value.str($moduleName)", Some(moduleName)) }
          copy(trees = valueTrees)
        }
      }
    }
  }

  class EnumExtractor {

    private def fromSumType(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {
      Option.whenever (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
        val coll = SumTypeEnumCollector(tpe)
        Option.whenever(coll.includesOnlySupportedSymbols()) {
          coll.resolved() map { collector =>
            U.Enum(tpe, collector.trees)
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
        val valueTrees = valueNames map { moduleName => (q"${N.internal.json}.Value.str($moduleName)", Some(moduleName)) }
        U.Enum(tpe, valueTrees)
      }
    }

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {
      fromScalaEnumeration(tpe) orElse fromSumType(tpe)
    }
  }

  val Enum = new EnumExtractor
}
