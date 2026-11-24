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

    /** A member that isn't a case class/object (the texts of Scala 3's `Sums`). `stack`: the path to the family, without the family itself. */
    private def unsupportedMember(root: Type, sym: Symbol, stack: List[Type]): Nothing = {
      val anonymousOrLocal = sym.name.decodedName.toString.startsWith("$anon") || isBlockLocal(sym)
      if (anonymousOrLocal) {
        // the family the subclass extends (a nested sub-family's, not the root's), as Scala 3 names it
        val parent = sym.asClass.baseClasses.drop(1).find(b => b.isClass && b.asClass.isSealed) match {
          case Some(p) if p != root.typeSymbol => p.asClass.toTypeConstructor
          case _                               => root
        }
        val p = showType(parent)
        c.abort(
          c.enclosingPosition,
          s"schema for $p is not supported${pathOf(stack)}: it has a subclass that is local to a block or anonymous (e.g. `new ${parent.typeSymbol.name.decodedName} {}`), " +
            s"which can't be a member of a derived family. Declare the subclasses at the top level or in an object, or provide an implicit json.Schema for $p"
        )
      } else
        c.abort(
          c.enclosingPosition,
          s"schema for ${showType(root)} is not supported${pathOf(stack)}: only case classes and case objects are supported as members of a sealed family, and ${showType(sym.asType.toTypeConstructor)} is neither. " +
            s"Make it one, or provide a json.schema.Predef or json.Schema for ${showType(root)}"
        )
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
          otherwise = sym => unsupportedMember(tpe, sym, ctx.stack.dropRight(1))
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
