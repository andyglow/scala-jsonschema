package com.github.andyglow.jsonschema

import scala.reflect.NameTransformer
import scala.reflect.internal.util.NoSourceFile
import scala.util.control.NonFatal

private[jsonschema] trait UProductTypes {
  this: UContext with UCommons with UScaladocs with UFieldDecorations with UScalaParsers =>
  import c.universe._

  // keyed by the decoded field name (`my-field`, not `my$minusfield`), as the fields are looked up
  private[UProductTypes] def fieldAnnotationMap(
    tpe: Type
  ): Map[String, List[c.universe.Annotation]] = {
    // annotations aimed at the underlying field (`@(description @field)`), read from the fields themselves (`a `, with the
    // local suffix). Not through the case accessor methods: a non-public val (`private val a`, `private[pkg]`, `protected`)
    // has a getter `a` that isn't a case accessor and a synthetic case accessor `a$access$0` that isn't a getter, whose
    // `accessed` fails an assertion (a compiler crash).
    def annotations0 = tpe.decls.collect {

      case s: TermSymbol if !s.isMethod && s.isCaseAccessor =>
        // workaround: force loading annotations
        s.typeSignature
        s.annotations.foreach(_.tree.tpe)

        s.name.decodedName.toString.trim -> s.annotations
    }.toMap

    // new implementation of annotation extractor
    def annotations1 =
      tpe.typeSymbol.asClass.primaryConstructor.typeSignature.paramLists.headOption flatMap { paramList: List[Symbol] =>
        Some(paramList.collect { case s =>
          s.name.decodedName.toString.trim -> s.annotations
        }.toMap)
      } getOrElse Map.empty

    // merged per name, the parameter's first: `annotations1` has an entry for every parameter, so a plain `++` dropped the
    // annotations aimed at the field (`@(description @field)`)
    val fields = annotations0
    val params = annotations1
    (fields.keySet ++ params.keySet).map(k => k -> (params.getOrElse(k, Nil) ++ fields.getOrElse(k, Nil))).toMap
  }

  private[UProductTypes] def resolveFields(tpe: Type): List[Field] = {
    val annotationMap       = fieldAnnotationMap(tpe)
    val subjectCompanionSym = tpe.typeSymbol
    // Scala 2 reflection gives no companion for a case class local to a block (it isn't a member of any owner's scope)
    val companionOrNone       = subjectCompanionSym.asClass.companion
    val isLocal               = companionOrNone == NoSymbol
    lazy val subjectCompanion = companionOrNone.asModule
    // a tree for `Companion.<name>`: the companion of a class nested in a class (a path-dependent type) has no static path, so it goes through the type's prefix (`h.Inner`,
    // `Holder.this.Inner`), as on Scala 3. Before, `<pkg>.Holder.Inner` didn't compile (`object Holder is not a member of package ...`).
    def companionMember(name: String): Tree =
      if (subjectCompanion.isStatic) parseFCQN(subjectCompanion.fullName + "." + name)
      else
        tpe match {
          case TypeRef(pre @ (_: ThisType | _: SingleType), _, _) => Select(internal.gen.mkAttributedRef(pre, subjectCompanion), TermName(name))
          case other                                              =>
            val name  = subjectCompanionSym.name.decodedName
            val outer = other match {
              case TypeRef(pre, _, _) => pre.typeSymbol.name.decodedName.toString
              case _                  => "Outer"
            }
            c.abort(
              c.enclosingPosition,
              s"default values of ${show(tpe)} are not supported: its companion $name is reached through a type projection, not a stable path. " +
                s"Use a path (`val o = new $outer; Json.schema[o.$name]`) or declare it in an object"
            )
        }

    def toField(fieldSym: TermSymbol, i: Int): Field = {
      val name       = NameTransformer.decode(fieldSym.name.toString)
      val fieldTpe   = fieldSym.typeSignature.dealias // In(tpe).dealias
      val isOption   = fieldTpe <:< T.option
      val hasDefault = fieldSym.isParamWithDefault
      if (hasDefault && isLocal)
        c.abort(
          c.enclosingPosition,
          s"default values of a case class local to a block are not supported on Scala 2: ${subjectCompanionSym.name.decodedName}.$name. " +
            s"Declare ${subjectCompanionSym.name.decodedName} in an object or at the top level"
        )

      // the field type with the type arguments of `tpe` substituted (`xs: List[T]` in `Foo[Int]` is a List[Int])
      def effectiveType = if (tpe.typeArgs.nonEmpty && tpe.typeSymbol.isClass) {
        resolveGenericType(fieldTpe, tpe.typeSymbol.asClass.typeParams, tpe.typeArgs)
      } else
        fieldTpe

      val default = Option.whenever(hasDefault) {
        val defaultGetterTree = companionMember("apply$default$" + (i + 1))
        val defaultGetterSym  =
          subjectCompanion.typeSignature.member(TermName(s"apply$$default$$${i + 1}")).asTerm
        val hasSource = defaultGetterSym.pos.source != NoSourceFile
        // we don't want to infer default value for `scala.None`
        val isNone = if (isOption) {
          val effectiveDefaultGetterTree =
            c.untypecheck {
              c.typecheck {
                if (!hasSource) {
                  // for continuous compilation
                  companionMember("apply$default$" + (i + 1))
                } else {
                  // for full-rebuild
                  val defaultGetterSym = subjectCompanion.typeSignature
                    .member(TermName(s"apply$$default$$${i + 1}"))
                    .asTerm
                  // we are in hasDefault block, so default.get should be ok
                  parseFCQN(parseParameter(defaultGetterSym).default.get)
                }
              }
            }

          try c.eval(c.Expr[Boolean](q"$effectiveDefaultGetterTree.isEmpty"))
          catch {
            case NonFatal(_) =>
              c.abort(
                c.enclosingPosition,
                s"Can't evaluate the default value of ${show(tpe)}.$name: ${show(fieldTpe)} at compile time, to tell None from Some. " +
                  s"On Scala 2 an Option default must be None, Some(<literal>) or a value of an already compiled module. It was:\n```\n${showCode(effectiveDefaultGetterTree)}\n```"
              )
          }
        } else false

        Some.when(!isNone) {
          val toV = c.inferImplicitValue(appliedType(T.toValue, effectiveType))
          if (toV.nonEmpty) q"Some($toV($defaultGetterTree))"
          else {
            val errorSuffix = {
              if (hasSource) try {
                val defaultGetterSym = subjectCompanion.typeSignature
                  .member(TermName(s"apply$$default$$${i + 1}"))
                  .asTerm
                val defaultValueCode = parseParameter(defaultGetterSym).default.get
                s"`$tpe(..., $name: $fieldTpe = $defaultValueCode)`."
              } catch {
                case err: Throwable =>
                  s"""`$tpe(..., $name: $fieldTpe)``.
                     |Can't parse source code defining the value: ${err.getMessage}.
                     |""".stripMargin
              }
              else {
                s"`$tpe(..., $name: $fieldTpe)`"
              }
            }

            c.abort(
              c.enclosingPosition,
              s"""Can't infer json value for default value of $errorSuffix
                 |Please provide `ToValue[$fieldTpe]` type class. It can also be automatically derived in case
                 |- your json library knows how to convert `$fieldTpe` into json and
                 |- the bridge between scala-jsonschema and your library is configured. Namely
                 | - corresponding library is in classpath (eg. scala-jsonschema-spray-json, scala-jsonschema-play-json, scala-jsonschema-circe-json, etc)
                 | - corresponding import is taking place (eg. `import com.github.andyglow.jsonschema.AsSpray._`, `import com.github.andyglow.jsonschema.AsPlay._`, `import com.github.andyglow.jsonschema.AsCirce._` etc)
                 |""".stripMargin
            )
          }
        }
      }

      val specifiedType =
        if (isOption) effectiveType.typeArgs.head
        else
          effectiveType

      Field(
        name = TermName(name),
        tpe = fieldTpe,
        effectiveTpe = specifiedType,
        annotations = annotationMap.getOrElse(name, List.empty),
        default = default,
        isOption = isOption,
        declaresDefault = hasDefault
      )
    }

    // this initializes defaults
    if (isLocal) {
      if (subjectCompanionSym.isAbstract) c.abort(c.enclosingPosition, s"No apply function found for ${subjectCompanionSym.fullName}")
    } else bestApply(subjectCompanion)

    val fields = tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption map { params =>
      params.map { _.asTerm }.zipWithIndex map { case (f, i) => toField(f, i) }
    }

    fields getOrElse List.empty
  }

  class CaseClassExtractor {
    import FieldDecorations._

    def unapply(tpe: Type)(implicit
      ctx: ResolutionContext,
      specFD: FieldDecorations = FieldDecorations.Empty
    ): Option[U.Obj] = {

      forNonValueCaseClass(tpe) {
        // scaladoc
        val scaladoc = getTypeScaladoc(tpe)

        // compute a field map
        val resolvedFields = resolveFields(tpe)

        // fields descriptions
        val scaladocFD = FieldDecorations.fromScaladoc(tpe, scaladoc)
        val fd         =
          FieldDecorations.fromFieldAnnotations(resolvedFields) ++
            scaladocFD ++
            specFD

        // descriptions given in the Json.objectSchema call must name fields
        if (ctx.isEmpty) specFD.validate(tpe, resolvedFields)
        // a stale scaladoc @param is only a warning, the same for the root and for nested types
        scaladocFD.warnUnknown(tpe, resolvedFields)

        // field titles (`@title` on a field): set on the field's schema
        val titles = FieldDecorations.titlesFromFieldAnnotations(resolvedFields)

        // compute field trees
        val fields = resolvedFields map { f =>
          val name = f.name.decodedName.toString

          // resolve the field type schema; an `Option[X]` field keeps the Option as an `option` node around X
          val optTpe      = appliedType(T.option.typeConstructor, List(f.effectiveTpe))
          val valueSchema = resolve(f.effectiveTpe, ctx :+ tpe)
          val typeSchema  = if (f.isOption) U.Opt(optTpe, f.effectiveTpe, valueSchema) else valueSchema
          val fieldSchema = titles.get(name).fold[SchemaType](typeSchema)(t => typeSchema.withExtra(typeSchema.extra.copy(title = Some(t))))
          val fieldTpe    = if (f.isOption) optTpe else f.effectiveTpe

          // description
          val description = fd.get(f.name.decodedName.toString)

          // RW Mode
          val rwMode = (f.hasAnnotation.readOnly, f.hasAnnotation.writeOnly) match {
            case (true, true)   => None
            case (false, false) => None
            case (true, false)  => Some(q"${N.Field}.RWMode.ReadOnly")
            case (false, true)  => Some(q"${N.Field}.RWMode.WriteOnly")
          }

          // create a field model
          f.default map { default =>
            // if default value is specified
            U.Obj.Field.FromJson(
              fieldTpe,
              name,
              fieldSchema,
              q"${!f.isOption && !f.hasDefault}",
              default,
              description,
              rwMode,
              hasDefault = f.declaresDefault
            )
          } getOrElse {
            // if no default is specified
            U.Obj.Field.Apply(
              fieldTpe,
              name,
              fieldSchema,
              Some(q"${!f.isOption && !f.hasDefault}"),
              None,
              description,
              rwMode,
              hasDefault = f.declaresDefault
            )
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
      specFD: FieldDecorations = FieldDecorations.Empty
    ): Option[CaseObjectSymbol] = {

      val sym = tpe.typeSymbol
      Some.when(sym.isClass && sym.isModuleClass) {
        CaseObjectSymbol(sym)
      }
    }
  }

  val CaseObject = new CaseObjectExtractor

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

      case x =>
        x.asTerm.alternatives.flatMap { apply =>
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
