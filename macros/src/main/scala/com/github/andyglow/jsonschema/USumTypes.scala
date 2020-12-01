package com.github.andyglow.jsonschema



private[jsonschema] trait USumTypes { this: UContext with UCommons with UValueTypes with UProductTypes with UImplicits with UTypeAnnotations with USignatures =>
  import c.universe._


  class SumTypeExtractor {

    private def isSealed(x: Type): Boolean = {
      val s = x.typeSymbol
      s.isClass && s.asClass.isSealed
    }

    private def isSupportedLeafType(x: Type): Boolean = {
      val s = x.typeSymbol
      s.isClass && !s.isModuleClass && s.asClass.isCaseClass
    }

    // BORROWED:
    // https://github.com/plokhotnyuk/jsoniter-scala/blob/3612fddf19a8ce23ac973d71e85ef02f79c06fff/jsoniter-scala-macros/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L351-L365
    private def collectRecursively(x: Type): Seq[Type] =
      if (x.typeSymbol.isClass) {
        val leafs = x.typeSymbol.asClass.knownDirectSubclasses.toSeq flatMap { s =>
          val cs = s.asClass
          val subTpe = if (cs.typeParams.isEmpty) cs.toType else resolveGenericType(cs.toType, cs.typeParams, x.typeArgs)
          if (isSealed(subTpe)) collectRecursively(subTpe)
          else if (isSupportedLeafType(subTpe)) Seq(subTpe)
          else c.abort(
            c.enclosingPosition,
            "Only Scala case classes are supported for Sum Type leafs. Please consider using of " +
              s"them for Sum Types with base '$x' or provide a custom implicitly accessible json.Schema for the Sum Type.")
        }
        if (isSupportedLeafType(x)) leafs :+ x else leafs
      } else Seq.empty

    def resolve(tpe: Type)(implicit ctx: ResolutionContext): Option[U.OneOf] = {
      Some.when (isSealed(tpe)) {
        // get type annotation for the root type
        // needed primarily for discriminator logic
        val rootTA = TypeAnnotations(tpe)
        val subTypes = collectRecursively(tpe)
        val schemas = subTypes map { subTpe =>
          val subSchema = Implicit.getOrElse(subTpe, subTpe match {
            case ValueClass(st) => st
            case CaseClass(st)  => st
            case _              => c.abort(c.enclosingPosition, "Only case classes and value classes are supported candidates for sum type hierarchy")
          })

          // if discriminator is specified we need to make several checks
          // 1. type must ne a product type
          // 2. if discriminator isn't a phantom, product must contain specified field
          rootTA.discriminator foreach { d =>
            def validate(t: SchemaType): Unit = t match {
              case o: SchemaType.Obj                     => if (!d.phantom && !o.fields.exists(_.name == d.field)) c.abort(c.enclosingPosition, s"Discriminator: Field '${d.field}' is not found in ${show(subTpe)}")
              case SchemaType.ValueClass(_, _, inner, _) => validate(inner)
              case _                                     => c.abort(c.enclosingPosition, "Discriminator: Only case classes and value classes are supported candidates for sum type hierarchy")
            }

            validate(subSchema)
          }

          // get hierarchy member type annotation
          // for discriminator-key
          val ta = TypeAnnotations(subTpe)

          // apply discriminator key if required
          val effectiveSubSchema = rootTA.discriminator.fold(subSchema) { d =>
            val key = ta.discriminatorKey match {
              case None                      => signature(subTpe)
              case Some(DiscriminatorKey(x)) => x
            }

            subSchema.withExtra(subSchema.extra.copy(discriminationKey = Some(key)))
          }

          // if `definition` annotation is specified wrap the schema into `def`
          ta.wrapIntoDefIfRequired(subTpe, effectiveSubSchema)
        }

        U.OneOf(tpe, schemas, rootTA.discriminator.map(_.field))
      }
    }

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.OneOf] =
      resolve(tpe)(ctx :+ tpe)
  }

  val SumType = new SumTypeExtractor
}
