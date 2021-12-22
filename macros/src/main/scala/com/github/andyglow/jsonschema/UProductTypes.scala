package com.github.andyglow.jsonschema

import scala.reflect.NameTransformer
import scala.reflect.internal.util.NoSourceFile


private[jsonschema] trait UProductTypes { this: UContext with UCommons with UScaladocs with UFieldDecorations with UScalaParsers =>
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
    val annotationMap       = fieldAnnotationMap(tpe)
    val subjectCompanionSym = tpe.typeSymbol
    val subjectCompanion    = subjectCompanionSym.asClass.companion.asModule

    def toField(fieldSym: TermSymbol, i: Int): Field = {
      val name        = NameTransformer.decode(fieldSym.name.toString)
      val fieldTpe    = fieldSym.typeSignature.dealias
      val isOption    = fieldTpe <:< T.option
      val hasDefault  = fieldSym.isParamWithDefault
      val default     = Option.whenever (hasDefault) {
        val defaultGetterTree = parseFCQN(subjectCompanion.fullName + ".apply$default$" + (i + 1))
        val defaultGetterSym  = subjectCompanion.typeSignature.member(TermName(s"apply$$default$$${i+1}")).asTerm
        val hasSource         = defaultGetterSym.pos.source != NoSourceFile
        // we don't want to infer default value for `scala.None`
        val isNone = if (isOption && !is211) {
          val effectiveDefaultGetterTree =
            c.untypecheck {
              c.typecheck {
                if (!hasSource) {
                  // for continuous compilation
                  parseFCQN(tpe.typeSymbol.fullName + ".apply$default$" + (i + 1))
                } else {
                  // for full-rebuild
                  val defaultGetterSym = subjectCompanion.typeSignature.member(TermName(s"apply$$default$$${i+1}")).asTerm
                  // we are in hasDefault block, so default.get should be ok
                  parseFCQN(parseParameter(defaultGetterSym).default.get)
                }
              }
            }

          try c.eval(c.Expr[Boolean](q"$effectiveDefaultGetterTree.isEmpty")) catch {
            case ex: Throwable =>
              throw new Exception(s"Unable to check isNone for ${show(tpe)}.${show(fieldSym)}: ${show(fieldTpe)}. May be try non-full rebuild. Throws on evaluation of:\n```\n${showCode(q"$effectiveDefaultGetterTree.isEmpty")}\n```", ex)
          }
        } else false

        Some.when (!isNone) {
          val toV = c.inferImplicitValue(appliedType(T.toValue, fieldTpe))
          if (toV.nonEmpty) q"Some($toV($defaultGetterTree))" else {
            val errorSuffix = {
              if (hasSource) try {
                val defaultGetterSym = subjectCompanion.typeSignature.member(TermName(s"apply$$default$$${i+1}")).asTerm
                val defaultValueCode = parseParameter(defaultGetterSym).default.get
                s"`$tpe(..., $name: $fieldTpe = $defaultValueCode)`."
              } catch {
                case err: Throwable =>
                  s"""`$tpe(..., $name: $fieldTpe)``.
                     |Can't parse source code defining the value: ${err.getMessage}.
                     |""".stripMargin
              } else {
                s"`$tpe(..., $name: $fieldTpe)`"
              }
            }

            c.abort(c.enclosingPosition,
              s"""Can't infer json value for default value of $errorSuffix
                 |Please provide `ToValue[$fieldTpe]` type class. It can also be automatically derived in case
                 |- your json library knows how to convert `$fieldTpe` into json and
                 |- the bridge between scala-jsonschema and your library is configured. Namely
                 | - corresponding library is in classpath (eg. scala-jsonschema-spray-json, scala-jsonschema-play-json, scala-jsonschema-circe-json, etc)
                 | - corresponding import is taking place (eg. `import com.github.andyglow.jsonschema.AsSpray._`, `import com.github.andyglow.jsonschema.AsPlay._`, `import com.github.andyglow.jsonschema.AsCirce._` etc)
                 |${if (isOption && is211) "NOTE: Functionality of recognizing `scala.None` is not available for scala 2.11." else ""}
                 |""".stripMargin)
          }
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
            FieldDecorations.fromScaladoc(tpe, scaladoc) ++
            specFD

        // check if all descriptions specified immediately in method call matches field names
        if (ctx.isEmpty) fd.validate(tpe, resolvedFields)

        // compute field trees
        val fields = resolvedFields map { f =>
          val name = f.name.decodedName.toString

          // resolve the field type schema
          val fieldSchema = {
            val schema = resolve(f.effectiveTpe, ctx :+ tpe)
            if (f.isOption && ctx.has[json.Profile.OptionAsArray]) {
              SchemaType.Arr(f.effectiveTpe, T.array.typeConstructor, schema, unique = true)
            } else
              schema
          }

          // description
          val description = fd.get(f.name.decodedName.toString)

          // RW Mode
          val rwMode = (f.hasAnnotation.readOnly, f.hasAnnotation.writeOnly) match {
            case (true, true)   => None
            case (false, false) => None
            case (true, false)  => Some(q"${N.Field}.RWMode.ReadOnly")
            case (false, true)  => Some(q"${N.Field}.RWMode.WriteOnly")
          }

          // required
          val required = if (f.isOption) ctx.has[json.Profile.OptionIsRequired] else !f.hasDefault

          // create a field model
          f.default map { default =>
            // if default value is specified
            U.Obj.Field.FromJson(f
              .effectiveTpe,
              name,
              fieldSchema,
              q"$required",
              default,
              description,
              rwMode)
          } getOrElse {
            // if no default is specified
            U.Obj.Field.Apply(
              f.effectiveTpe,
              name,
              fieldSchema,
              q"$required",
              None,
              description,
              rwMode)
          }
        }

        U.Obj(tpe, fields)
      }
    }
  }

  val CaseClass = new CaseClassExtractor

  case class CaseObjectSymbol(sym: Symbol)

  class CaseObjectExtractor {

    def unapply(tpe: Type)(implicit
      ctx: ResolutionContext,
      specFD: FieldDecorations = FieldDecorations.Empty): Option[CaseObjectSymbol] = {

      val sym = tpe.typeSymbol
      Some.when (sym.isClass && sym.isModuleClass) {
        CaseObjectSymbol(sym)
      }
    }
  }

  val CaseObject = new CaseObjectExtractor

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
