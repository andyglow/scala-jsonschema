package com.github.andyglow.jsonschema

import scala.reflect.NameTransformer


private[jsonschema] trait UProductTypes { this: UContext with UCommons with UScaladocs with UFieldDecorations =>
  import c.universe._


  private[UProductTypes] def fieldAnnotationMap(tpe: Type): Map[String, List[c.universe.Annotation]] = {
    // old implementation of annotation extractor
    def annotations0 = tpe.decls.collect {

      case s: MethodSymbol if s.isCaseAccessor =>
        // workaround: force loading annotations
        s.typeSignature
        s.accessed.annotations.foreach(_.tree.tpe)

        s.name.toString.trim -> s.accessed.annotations
    }.toMap

    // new implementation of annotation extractor
    def annotations1 = tpe.typeSymbol.asClass.primaryConstructor.typeSignature.paramLists.headOption flatMap { paramList: List[Symbol] =>
      Some(paramList.collect {
        case s => s.name.toString.trim -> s.annotations
      }.toMap)
    } getOrElse Map.empty

    annotations0 ++ annotations1
  }

  private[UProductTypes] def resolveFields(tpe: Type): Seq[Field] = {
    val annotationMap = fieldAnnotationMap(tpe)

    val subjectCompanionSym = tpe.typeSymbol
    val subjectCompanion    = subjectCompanionSym.companion

    def toField(fieldSym: TermSymbol, i: Int): Field = {
      val name        = NameTransformer.decode(fieldSym.name.toString)
      val fieldTpe    = fieldSym.typeSignature.dealias // In(tpe).dealias
      val isOption    = fieldTpe <:< T.option
      val hasDefault  = fieldSym.isParamWithDefault
      val toV         = c.inferImplicitValue(appliedType(T.toValue, fieldTpe))
      val default     = Some.when (hasDefault) {
        val getter = TermName("apply$default$" + (i + 1))
        if (toV.nonEmpty) q"Some($toV($subjectCompanion.$getter))" else {
          c.abort(c.enclosingPosition, s"Can't infer a json value for '$name': $fieldTpe")
        }
      }

      def effectiveType = if (tpe.typeArgs.nonEmpty && tpe.typeSymbol.isClass) {
        resolveGenericType(
          fieldTpe,
          tpe.typeSymbol.asClass.typeParams,
          tpe.typeArgs)
      } else
        fieldTpe

      val specifiedType =
        if (isOption) effectiveType.typeArgs.head
        else
          effectiveType

      Field(
        name          = TermName(name),
        tpe           = fieldTpe,
        effectiveTpe  = specifiedType,
        annotations   = annotationMap.getOrElse(name, List.empty),
        default       = default,
        isOption      = isOption)
    }

    // this initializes defaults
    bestApply(subjectCompanion)

    val fields = tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption map { params =>
      params.map { _.asTerm }.zipWithIndex map { case (f, i) => toField(f, i) }
    }

    fields getOrElse Seq.empty
  }



  class CaseClassExtractor {
    import FieldDecorations._

    def unapply(tpe: Type)(implicit
      ctx: ResolutionContext,
      specFD: FieldDecorations = FieldDecorations.Empty): Option[U.Obj] = {

      forNonValueCaseClass(tpe) {
        // scaladoc
        val scaladoc = getTypeScaladoc(tpe)

        // compute a field map
        val resolvedFields = resolveFields(tpe)

        // fields descriptions
        val fd =
          FieldDecorations.fromFieldAnnotations(resolvedFields) ++
            FieldDecorations.fromScaladoc(scaladoc) ++
            specFD

        // check if all descriptions specified immediately in method call matches field names
        if (ctx.isEmpty) fd.validate(tpe, resolvedFields)

        // compute field trees
        val fields = resolvedFields map { f =>
          val name = f.name.decodedName.toString

          // resolve the field type schema
          val fieldSchema = resolve(f.effectiveTpe, ctx :+ tpe)

          // description
          val description = fd.get(f.name.decodedName.toString)

          // create a field model
          f.default map { default =>
            // if default value is specified
            U.Obj.Field.FromJson(f
              .effectiveTpe,
              q"$name",
              fieldSchema,
              q"${!f.isOption && !f.hasDefault}",
              default,
              description)
          } getOrElse {
            // if no default is specified
            U.Obj.Field.Apply(
              f.effectiveTpe,
              q"$name",
              fieldSchema,
              Some(q"${!f.isOption && !f.hasDefault}"),
              None,
              description)
          }
        }

        U.Obj(tpe, fields)
      }
    }
  }

  val CaseClass = new CaseClassExtractor

//
//  object CaseClass {
//
//
//
//    def gen(cc: CaseClass, tpe: Type, ctx: ResolutionContext): Tree = {
//      val fieldMap = cc.fields
//      // check if all descriptions specified immediately in method call matches field names
//      if (ctx.isEmpty) descriptions foreach { descriptions =>
//        descriptions.keySet foreach { k =>
//          if (!fieldMap.exists(_.name.decodedName.toString == k))
//            c.abort(c.enclosingPosition, s"unknown field: $k. ${show(tpe)} fields are: ${fieldMap.map(_.name.decodedName.toString).mkString(", ")}")
//        }
//      }
//
//      val scaladoc = getTypeScaladoc(tpe)
//      val objTitle = cc.title
//      val objDescr = scaladoc flatMap { _.description } orElse cc.description
//      val obj = q"$schemaObj.`object`"
//      val fields = fieldMap map { f =>
//        val name      = f.name.decodedName.toString
//        val jsonType  = resolve(f.effectiveTpe, if (f.isOption) ctx else ctx :+ tpe)
//        val fDescr    = {
//          def fromScaladoc   = scaladoc flatMap { _.param(name) }
//          def fromAnnotation = f.annotations
//            .map(_.tree)
//            .filter(_.tpe <:< typeOf[json.schema.description])
//            .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }
//          def fromSpec       = if (ctx.isEmpty) descriptions flatMap { _.get(name) } else None
//
//          fromSpec orElse fromScaladoc orElse fromAnnotation
//        }
//
//        val tree = f.default map { d =>
//          q"$obj.Field.fromJson[${f.effectiveTpe}](name = $name, tpe = $jsonType, required = ${ !f.isOption && !f.hasDefault }, default = $d)"
//        } getOrElse {
//          q"$obj.Field[${f.effectiveTpe}](name = $name, tpe = $jsonType, required = ${ !f.isOption && !f.hasDefault })"
//        }
//        fDescr match {
//          case Some(descr) => q"$tree.withDescription(Some($descr))"
//          case None => tree
//        }
//      }
//
//      (objDescr, objTitle) match {
//        case (Some(descr), Some(title)) => q"$obj[$tpe](..$fields).duplicate(description = Some($descr), title = Some($title))"
//        case (_, Some(title))           => q"$obj[$tpe](..$fields).duplicate(title = Some($title))"
//        case (Some(descr), _)           => q"$obj[$tpe](..$fields).duplicate(description = Some($descr))"
//        case _                          => q"$obj[$tpe](..$fields)"
//      }
//    }
//  }


  private[UProductTypes] def bestApply(sym: Symbol): Option[MethodSymbol] = {
    val tpe = sym.typeSignature

    tpe.decl(TermName("apply")) match {

      case NoSymbol =>
        c.abort(c.enclosingPosition, s"No apply function found for ${sym.fullName}")

      case x => x.asTerm.alternatives.flatMap { apply =>
        val method = apply.asMethod

        def areAllImplicit(pss: List[List[Symbol]]): Boolean = pss forall {
          case p :: _ => p.isImplicit
          case _      => false
        }

        method.paramLists match {
          case ps :: pss if ps.nonEmpty && areAllImplicit(pss) => Some(method)
          case _                                               => None
        }
      }.headOption
    }
  }

}
