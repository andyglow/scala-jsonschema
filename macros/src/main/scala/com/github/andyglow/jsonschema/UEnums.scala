package com.github.andyglow.jsonschema

private[jsonschema] trait UEnums {
  this: UContext with UCommons with UJsonValueType with UTypeAnnotations with UFlags =>
  import c.universe._

  private val ScalaEnumTpe = typeOf[scala.Enumeration]

  case class EnumFamily(tpe: Type, items: Seq[EnumItem])(implicit ctx: ResolutionContext) {

    lazy val rootTypeHint: Type = TypeAnnotations(tpe).typeHint

    lazy val singleSchema: SchemaType = {
      val ss = schemas.keySet
      if (ss.isEmpty) {
        abort(s"Error inferring schema for enum: ${show(tpe)}. No items defined")
      } else if (ss.size > 1) {
        val details = ss.toSeq
          .sortBy { schema => showCode(schema.tree) }
          .map { schema =>
            val schemaDetails = schemas(schema)
              .sortBy { member => show(member.tpe) }
              .map { member =>
                s"  - ${show(member.tpe)}.${if (member.typeHint != NoType) s" Type Hint: ${show(member.typeHint)}"
                else ""}"
              }
              .mkString("\n")
            s"- ${showCode(schema.tree)}:\n$schemaDetails"
          }
          .mkString("\n")
        abort(
          s"Error inferring schema for enum: ${show(tpe)}. Some family members come with different schemas:\n$details"
        )
      } else
        ss.head
    }

    lazy val schemas: Map[SchemaType, Seq[EnumItem]] = {
      val schemas = for {
        enumItem <- items
        schema = enumItem.typeHint
                   .orElse(rootTypeHint)
                   .opt
                   .map(resolve(_, ctx)) orElse JsonValueType.unapply(enumItem.schemaTree)
      } yield {
        val effectiveSchema = schema match {
          case None =>
            enumItem match {
              case _: EnumItem.FromToValue =>
                abort {
                  s"""${show(enumItem.tpe)}: In order to convert this into enum item
                     |the combination of ToValue and Writer/Encoder (from your json-library) is going to be used
                     |```
                     |${showCode(enumItem.schemaTree)}
                     |```
                     |This is the case when resulting json value is hard to reason about in compile time.
                     |Use @typeHint annotation to specify correct type.
                     |""".stripMargin
                }
              case _ =>
            }

            SchemaType.Str(enumItem.tpe, q"None")
          case Some(x) => x
        }

        (effectiveSchema, enumItem)
      }

      schemas
        .groupBy { case (schema, _) => schema }
        .map { case (schema, tuples) => (schema, tuples map { case (_, item) => item }) }
    }
  }

  sealed trait EnumItem {
    // when enum is created from class name, we know it's name for sure
    def knownString: Option[String]
    def tpe: Type
    def schemaTree: Tree
    def typeAnnotations: TypeAnnotations
    def tuple: (Tree, Option[String]) = (schemaTree, knownString)
    def typeHint: Type                = typeAnnotations.typeHint
  }
  object EnumItem {
    case class FromCaseObject(tpe: Type, value: String, typeAnnotations: TypeAnnotations) extends EnumItem {
      def knownString: Option[String] = Some(value)
      def schemaTree: Tree            = q"${N.internal.json}.Value.str($value)"
    }

    case class FromToValue(tpe: Type, value: Tree, typeAnnotations: TypeAnnotations) extends EnumItem {
      def knownString: Option[String] = None
      def schemaTree: Tree            = value
    }
  }

  // ISSUE: https://github.com/andyglow/scala-jsonschema/issues/106
  // Used to extract case objects from sealed trait hierarchy
  class SumTypeEnumExtractor(tpe: Type) {

    private val toValueTree =
      c.inferImplicitValue(appliedType(T.toValue, tpe), silent = true, withMacrosDisabled = true)

    private lazy val items: Seq[Type] =
      resolveSumTypeRecursively(tpe, include = isCaseObject, otherwise = _ => NoType)

    def allResolved(silent: Boolean = false): Option[Seq[EnumItem]] = {
      val invalid = items.contains(NoType)
      if (invalid) {
        if (!silent)
          abort(
            s"Only Scala case objects are supported for Sum Type leaves\nPlease consider using of " +
              s"them for Sum Types with base '$tpe' or provide a custom implicitly accessible json.Schema for the Sum Type."
          )
        else None
      } else {
        val symbols = items.map(_.typeSymbol)
        someResolved(symbols)
      }
    }

    def someResolved(symbols: Seq[Symbol]): Option[Seq[EnumItem]] = {
      Option.whenever(symbols forall isCaseObject) {
        if (toValueTree.nonEmpty) {
          symbols collect { case module: ClassSymbol =>
            val caseObj = module.owner.asClass.toType.decls.find { d =>
              d.name == module.name.toTermName
            } getOrElse NoSymbol
            val tpe = module.toType
            EnumItem.FromToValue(tpe, q"$toValueTree($caseObj)", TypeAnnotations(tpe))
          }
        } else {
          symbols collect { case module: ClassSymbol =>
            val tpe = module.toType
            EnumItem.FromCaseObject(tpe, module.name.decodedName.toString, TypeAnnotations(tpe))
          }
        }
      }
    }
  }

  class EnumExtractor {

    private def fromSumType(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] = {
      Option.whenever(tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
        val extractor = new SumTypeEnumExtractor(tpe)
        extractor.allResolved(silent = true) map { enumItems =>
          val family = EnumFamily(tpe, enumItems)
          var schema = family.singleSchema
          val hasFromToValueMembers = enumItems collectFirst { case _: EnumItem.FromToValue =>
            true
          } getOrElse false

          if (!hasFromToValueMembers) {
            schema = SchemaType.Str(tpe, q"None")
            family.rootTypeHint.opt.filterNot(_ =:= typeOf[String]) foreach { hintTpe =>
              warn(
                s"@typeHint[$hintTpe] is ignored for $tpe, as all family members was derived directly from case objects. Enum Type is String"
              )
            }
            enumItems foreach {
              case EnumItem.FromCaseObject(tpe, _, hintAnnotation)
                  if hintAnnotation.typeHint != NoType && !(hintAnnotation.typeHint =:= typeOf[
                    String
                  ]) =>
                warn(
                  s"@typeHint[${hintAnnotation.typeHint}] is ignored for $tpe, as all family members was derived directly from case objects. Enum Type is String"
                )
              case _ =>
            }
          }

          if (flags.enumsAsOneOf) {
            U.OneOf(
              tpe = tpe,
              memberSchema = enumItems map { item =>
                U.Const(item.tpe, schema, item.schemaTree).withTypeAnnotations(item.typeAnnotations)
              },
              discriminatorField = None
            )
          } else
            U.Enum(tpe, schema, enumItems.map(_.tuple))
        }
      }
    }

    private def fromScalaEnumeration(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Enum] = {

      // Given that `tpe` can refer to different aspects of enum
      // - WeekDay.type
      // - WeekDay.Value
      // - WeekDay.AliasedTpe. eg. `type AliasedTpe = Value`
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
        val valueTrees = valueNames map { moduleName =>
          (q"${N.internal.json}.Value.str($moduleName)", Some(moduleName))
        }
        U.Enum(tpe, SchemaType.Str(tpe, q"None"), valueTrees)
      }
    }

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] = {
      fromScalaEnumeration(tpe) orElse fromSumType(tpe)
    }
  }

  val Enum = new EnumExtractor
}
