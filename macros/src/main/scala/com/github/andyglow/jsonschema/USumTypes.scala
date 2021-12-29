package com.github.andyglow.jsonschema

import com.github.andyglow.scalamigration._

private[jsonschema] trait USumTypes {
  this: UContext with UCommons with UFlags with UValueTypes with UProductTypes with UImplicits with UTypeAnnotations with USignatures with UEnums =>
  import c.universe._

  class SumTypeExtractor {

    private def isSupportedLeafSymbol(x: Symbol): Boolean = {
      x.isClass && x.asClass.isCaseClass
    }
    private def isSupportedLeafType(x: Type): Boolean = {
      val s = x.typeSymbol
      isSupportedLeafSymbol(s)
    }

    private sealed trait Extraction
    private object Extraction {
      private[USumTypes] case class Simple(tpe: Type, schema: SchemaType) extends Extraction
      private[USumTypes] case class Enumerated(
        tpe: Type,
        schema: SchemaType,
        members: Seq[EnumItem]
      ) extends Extraction
    }

    def resolveOneOf(tpe: Type)(implicit ctx: ResolutionContext): Option[U.OneOf] = {
      Some.when(isSealed(tpe)) {
        // get type annotation for the root type
        // needed primarily for discriminator logic
        val rootTA = TypeAnnotations(tpe)

        // resolve all sealed hierarchy members
        val subTypes = resolveSumTypeRecursively(
          tpe,
          include = isSupportedLeafType,
          otherwise = sym =>
            abort(
              "Supported Type:" + isSupportedLeafSymbol(sym) +
                s"\nOnly Scala case classes/objects are supported for Sum Type leaves.\nActual: ${showRaw(sym)}.\nPlease consider using of " +
                s"them for Sum Types with base '$tpe' or provide a custom implicitly accessible json.Schema for the Sum Type."
            )
        )

        // this needs to support hybrid mode where hierarchy might
        // include both case classes and case objects at the same time
        val enumExtractor            = new SumTypeEnumExtractor(tpe)
        var enumItems: Seq[EnumItem] = Seq.empty

        val subExtractions = subTypes map { subTpe =>
          // get hierarchy member type annotation
          // for discriminator-key and the rest
          val subTA = TypeAnnotations(subTpe)

          val subExtraction: Extraction = Implicit.get(subTpe) map {
            Extraction.Simple(subTpe, _)
          } getOrElse {
            subTpe match {
              case ValueClass(st) => Extraction.Simple(subTpe, st.withTypeAnnotations(subTA))
              case CaseClass(st)  => Extraction.Simple(subTpe, st.withTypeAnnotations(subTA))
              case CaseObject(co) if {
                    enumItems = enumExtractor.someResolved(Seq(co.sym)) getOrElse Seq.empty
                    enumItems.nonEmpty
                  } =>
                // oneof and enum at this point are used as containers..
                // there is a code down below that flattens enums list into one instance
                val schema = EnumFamily(tpe, enumItems).singleSchema
                Extraction.Enumerated(tpe, schema, enumItems)
              case _ =>
                c.abort(
                  c.enclosingPosition,
                  "Only case classes/objects and value classes are supported candidates for sum type hierarchy"
                )
            }
          }

          // if discriminator is specified we need to make several checks
          // 1. type must be a product type
          // 2. if discriminator isn't a phantom, product must contain specified field
          rootTA.discriminator foreach { d =>
            def validate(t: SchemaType): Unit = t match {
              case o: SchemaType.Obj =>
                if (!d.phantom && !o.fields.exists(_.name == d.field))
                  c.abort(
                    c.enclosingPosition,
                    s"Discriminator: Field '${d.field}' is not found in ${show(subTpe)}"
                  )
              case SchemaType.ValueClass(_, _, inner, _) => validate(inner)
              case SchemaType.Enum(_, _, _, _)           => // skip
              case _ =>
                c.abort(
                  c.enclosingPosition,
                  "Discriminator: Only case classes/objects and value classes are supported candidates for sum type hierarchy"
                )
            }

            subExtraction match {
              case Extraction.Simple(_, subSchema) => validate(subSchema)
              case _                               =>
            }
          }

          // apply discriminator key if required
          val effectiveSubExtraction = rootTA.discriminator.fold(subExtraction) { d =>
            val key = subTA.discriminatorKey match {
              case None                      => signature(subTpe)
              case Some(DiscriminatorKey(x)) => x
            }

            subExtraction match {
              case Extraction.Simple(tpe, subSchema) =>
                Extraction.Simple(
                  tpe,
                  subSchema.withExtra(subSchema.extra.copy(discriminationKey = Some(key)))
                )
              case x => x
            }
          }

          // if `definition` annotation is specified wrap the schema into `def`
          // Even if this is an Enum. Let user decide. It may result in several definitions of enum containing only one element
          effectiveSubExtraction match {
            case Extraction.Simple(tpe, effectiveSubSchema) =>
              Extraction.Simple(tpe, subTA.wrapIntoDefIfRequired(subTpe, effectiveSubSchema))
            case x => x
          }
        }

        // - if enums presented as one-of: convert all items into `const`
        // - if enums come as is: flatten same-schema enums into one big enum
        val effectiveSchemas = subExtractions.foldLeft[Seq[SchemaType]](Nil) {
          case (acc, Extraction.Simple(_, schema)) => acc :+ schema
          case (acc, Extraction.Enumerated(tpe, schema, items)) =>
            if (flags.enumsAsOneOf) {
              items.foldLeft(acc) { case (acc, i) =>
                acc :+ U.Const(tpe, schema, i.schemaTree).withTypeAnnotations(i.typeAnnotations)
              }
            } else {
              // find enum (match by schema)
              // if exists, we don't want to duplicate enums, but rather collect all the enum-items under one umbrella
              val existingEnum = acc.collectFirst {
                case x @ U.Enum(_, s, _, _) if s == schema => x
              }
              existingEnum match {
                case Some(enum) =>
                  val shrinkedAcc = acc.filter {
                    case U.Enum(_, s, _, _) if s == schema => false
                    case _                                 => true
                  }
                  shrinkedAcc :+ U.Enum(tpe, schema, enum.values ++ items.map { _.tuple })
                case None => acc :+ U.Enum(tpe, schema, items.map { _.tuple })
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
