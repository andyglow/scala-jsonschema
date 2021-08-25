package com.github.andyglow.jsonschema

import com.github.andyglow.scalamigration._


private[jsonschema] trait USumTypes { this: UContext with UCommons with UValueTypes with UProductTypes with UImplicits with UTypeAnnotations with USignatures with UEnums =>
  import c.universe._


  class SumTypeExtractor {

    private def isSupportedLeafSymbol(x: Symbol): Boolean = {
      x.isClass && x.asClass.isCaseClass
    }
    private def isSupportedLeafType(x: Type): Boolean = {
      val s = x.typeSymbol
      isSupportedLeafSymbol(s)
    }

    def resolveOneOf(tpe: Type)(implicit ctx: ResolutionContext): Option[U.OneOf] = {
      Some.when (isSealed(tpe)) {
        // get type annotation for the root type
        // needed primarily for discriminator logic
        val rootTA = TypeAnnotations(tpe)

        // resolve all sealed hierarchy members
        val subTypes = resolveSumTypeRecursively(
          tpe,
          include = isSupportedLeafType,
          otherwise = sym => abort(
            "Supported Type:" + isSupportedLeafSymbol(sym) +
              s"\nOnly Scala case classes/objects are supported for Sum Type leaves.\nActual: ${showRaw(sym)}.\nPlease consider using of " +
              s"them for Sum Types with base '$tpe' or provide a custom implicitly accessible json.Schema for the Sum Type."))

        // this needs to support hybrid mode where hierarchy might
        // include both case classes and case objects at the same time
        val enumExtractor = new SumTypeEnumExtractor(tpe)
        var enumItems: Seq[EnumItem] = Seq.empty

        val schemas = subTypes map { subTpe =>
          val subSchema = Implicit.getOrElse(subTpe, subTpe match {
            case ValueClass(st) => st
            case CaseClass(st)  => st
            case CaseObject(co) if
              { enumItems = enumExtractor.someResolved(Seq(co.sym)) getOrElse Seq.empty
                enumItems.nonEmpty
              } => U.Enum(tpe, EnumFamily(tpe, enumItems).singleSchema, enumItems.map(_.tuple))
            case _              => c.abort(c.enclosingPosition, "Only case classes/objects and value classes are supported candidates for sum type hierarchy")
          })

          // if discriminator is specified we need to make several checks
          // 1. type must be a product type
          // 2. if discriminator isn't a phantom, product must contain specified field
          rootTA.discriminator foreach { d =>
            def validate(t: SchemaType): Unit = t match {
              case o: SchemaType.Obj                     => if (!d.phantom && !o.fields.exists(_.name == d.field)) c.abort(c.enclosingPosition, s"Discriminator: Field '${d.field}' is not found in ${show(subTpe)}")
              case SchemaType.ValueClass(_, _, inner, _) => validate(inner)
              case SchemaType.Enum(_, _, _, _)           => // skip
              case _                                     => c.abort(c.enclosingPosition, "Discriminator: Only case classes/objects and value classes are supported candidates for sum type hierarchy")
            }

            validate(subSchema)
          }

          // get hierarchy member type annotation
          // for discriminator-key
          val subTA = TypeAnnotations(subTpe)

          // apply discriminator key if required
          val effectiveSubSchema = rootTA.discriminator.fold(subSchema) { d =>
            val key = subTA.discriminatorKey match {
              case None                      => signature(subTpe)
              case Some(DiscriminatorKey(x)) => x
            }

            subSchema.withExtra(subSchema.extra.copy(discriminationKey = Some(key)))
          }

          // if `definition` annotation is specified wrap the schema into `def`
          // Even if this is an Enum. Let user decide. It may result in several definitions of enum containing only one element
          subTA.wrapIntoDefIfRequired(subTpe, effectiveSubSchema)
        }

        // flatten schemas
        // gather all enums under one definition
        val enumTrees = schemas.foldLeft[Map[SchemaType, Seq[(Tree, Option[String])]]](Map.empty) {
          case (acc, U.Enum(_, schema, values, _)) => acc.updatedWith(schema) { _.fold(values) { _ ++ values } }
          case (acc, z)                            => acc
        }

        val effectiveSchemas = if (enumTrees.isEmpty) schemas else {
          schemas.filter { case _: U.Enum => false; case _ => true } ++ {
            enumTrees map { case (st, trees) =>
              U.Enum(tpe, st, trees)
            }
          }
        }

        U.OneOf(tpe, effectiveSchemas, rootTA.discriminator.map(_.field))
      }
    }

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.OneOf] =
      resolveOneOf(tpe)(ctx :+ tpe)
  }

  val SumType = new SumTypeExtractor
}
