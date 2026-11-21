package com.github.andyglow.jsonschema

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

    def resolveOneOf(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] = {
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

        val enumExtractor = new SumTypeEnumExtractor(tpe)
        val isEither      = tpe.typeSymbol == typeOf[Either[_, _]].typeSymbol

        // one member per leaf (nested sealed families flattened), in declaration order: the facts, no shaping
        val members = subTypes map { subTpe =>
          val subTA                                                                                                                            = TypeAnnotations(subTpe)
          val sym                                                                                                                              = subTpe.typeSymbol
          val name                                                                                                                             = sym.name.decodedName.toString
          def member(kind: U.Family.Kind, schema: SchemaType, value: Option[Tree] = None, knownString: Option[String] = None): U.Family.Member =
            U.Family.Member(
              name = name,
              fullName = sym.fullName,
              sig = signature(subTpe),
              kind = kind,
              schema = schema,
              value = value,
              cls = Some(q"_root_.scala.Predef.classOf[$subTpe]"),
              texts = subTA.texts,
              discriminatorKey = subTA.discriminatorKey.map(_.value),
              definition = subTA.definition,
              knownString = knownString
            )

          Implicit.get(subTpe) map { member(U.Family.Kind.Provided, _) } getOrElse {
            subTpe match {
              case ValueClass(st) =>
                val field = sym.asClass.primaryConstructor.asMethod.paramLists.flatten.headOption.map(_.name.decodedName.toString)
                member(U.Family.Kind.ValueClass, st).copy(valueField = field)
              case CaseClass(st)                                                                => member(U.Family.Kind.Product, st)
              case CaseObject(co) if enumExtractor.someResolved(Seq(co.sym)).exists(_.nonEmpty) =>
                val item = enumExtractor.someResolved(Seq(co.sym)).get.head
                // the value schema 0.7 used for this singleton (EnumFamily's single schema of one item)
                member(U.Family.Kind.Singleton, EnumFamily(tpe, Seq(item)).schemaOf(item), Some(item.schemaTree), item.knownString)
              case _ =>
                c.abort(
                  c.enclosingPosition,
                  "Only case classes/objects and value classes are supported candidates for sum type hierarchy"
                )
            }
          }
        }

        // if discriminator is specified we need to make several checks
        // 1. type must be a product type
        // 2. if discriminator isn't a phantom, product must contain specified field
        rootTA.discriminator foreach { d =>
          def validate(subTpe: Type, t: SchemaType): Unit = t match {
            case o: SchemaType.Obj =>
              if (!d.phantom && !o.fields.exists(_.name == d.field))
                c.abort(
                  c.enclosingPosition,
                  s"Discriminator: Field '${d.field}' is not found in ${show(subTpe)}"
                )
            case SchemaType.ValueClass(_, _, inner, _) => validate(subTpe, inner)
            case _                                     =>
              c.abort(
                c.enclosingPosition,
                "Discriminator: Only case classes/objects and value classes are supported candidates for sum type hierarchy"
              )
          }

          subTypes.zip(members) foreach { case (subTpe, m) => if (m.kind != U.Family.Kind.Singleton) validate(subTpe, m.schema) }
        }

        if (isEither) eitherOneOf(tpe, subTypes zip members) else U.Family(tpe, members.toList, rootTA.discriminator, flags.enumsAsOneOf)
      }
    }

    /** `Either` stays a plain `oneOf` (pinned by the goldens): its members with their annotations. */
    private def eitherOneOf(tpe: Type, members: Seq[(Type, U.Family.Member)]): SchemaType =
      U.OneOf(
        tpe,
        members map { case (subTpe, m) =>
          val subTA = TypeAnnotations(subTpe)
          val typed = m.kind match {
            case U.Family.Kind.Provided => m.schema
            case _                      => m.schema.withTypeAnnotations(subTA)
          }
          subTA.wrapIntoDefIfRequired(subTpe, typed)
        },
        None
      )

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] =
      resolveOneOf(tpe)(ctx :+ tpe)
  }

  val SumType = new SumTypeExtractor
}
