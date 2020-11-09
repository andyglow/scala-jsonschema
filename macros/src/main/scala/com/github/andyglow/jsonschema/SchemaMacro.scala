package com.github.andyglow.jsonschema

import com.github.andyglow.json.ToValue
import com.github.andyglow.scaladoc.{Scaladoc, SlowParser}
import json.Schema.`dictionary`.KeyPattern

import scala.reflect.NameTransformer
import scala.reflect.internal.util.NoSourceFile
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal


object SchemaMacro {

  def derivePredef[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[json.schema.Predef[T]] = {
    import c.universe._

    val schema = deriveInternal[T, json.Schema](c)
    c.Expr[json.schema.Predef[T]](q"_root_.json.schema.Predef($schema)")
  }

  def deriveSchema[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[json.Schema[T]] = {
    deriveInternal[T, json.Schema](c)
  }

  def deriveObjectSchema[T : c.WeakTypeTag](c: blackbox.Context)(descriptions: c.Expr[(String, String)]*): c.Expr[json.Schema.`object`[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val symbol = tpe.typeSymbol

    if (symbol.isClass) {
      val clazz = symbol.asClass
      if (clazz.isCaseClass) {
        if (clazz.isDerivedValueClass) {
          c.abort(c.enclosingPosition, "Json.objectSchema can't be used against value classes")
        } else {
          def fromTree(x: Tree): String = x match {
            case Literal(Constant(v: String)) => v
            case _                            => c.abort(c.enclosingPosition, """only constant Strings are allowed in Description specification. Example: "id" -> "Record ID", ... """)
          }
          val descrs = descriptions.map { d =>
            // c.info(c.enclosingPosition, "DESCR: " + showRaw(d.tree), force = true)
            d.tree match {
              // case "key" -> "val" // scala 2.12
              case Apply(TypeApply(Select(Apply(TypeApply(Select(Select(Ident(TermName("scala")), TermName("Predef")), TermName("ArrowAssoc")), List(TypeTree())), List(k)), TermName("$minus$greater")), List(TypeTree())), List(v)) => (fromTree(k), fromTree(v))
              // case "key" -> "val" // scala 2.11
              case Apply(TypeApply(Select(Apply(TypeApply(Select(Select(This(TypeName("scala")), TermName("Predef")), TermName("ArrowAssoc")), List(TypeTree())), List(k)), TermName("$minus$greater")), List(TypeTree())), List(v))  => (fromTree(k), fromTree(v))
              // case ("key", "val")
              case Apply(TypeApply(Select(Select(Ident(TermName("scala")), TermName("Tuple2")), TermName("apply")), List(TypeTree(), TypeTree())), List(k, v))                                                                        => (fromTree(k), fromTree(v))
            }
          }.toMap

          deriveInternal[T, json.Schema.`object`](c, Some(descrs))
        }
      } else
        c.abort(c.enclosingPosition, "Json.objectSchema can't be used against non-case classes")
    } else
      c.abort(c.enclosingPosition, "Json.objectSchema can be used only against case classes")
  }

  private def deriveInternal[T: c.WeakTypeTag, S[_]](c: blackbox.Context, descriptions: Option[Map[String, String]] = None): c.Expr[S[T]] = {
    import c.universe._

    val jsonPkg       = q"_root_.json"
    val intJsonPkg    = q"_root_.com.github.andyglow.json"
    val schemaObj     = q"$jsonPkg.Schema"
    val validationObj = q"$jsonPkg.Validation"

    val subject               = weakTypeOf[T]
    val optionTypeCons        = weakTypeOf[Option[_]]
    val toValueTypeCons       = weakTypeOf[ToValue[_]]
    val setTypeCons           = weakTypeOf[Set[_]]
    val mapTypeCons           = weakTypeOf[Map[_, _]]
    val schemaTypeConstructor = typeOf[json.Schema[_]].typeConstructor
    val predefTypeConstructor = typeOf[json.schema.Predef[_]].typeConstructor
    val mapKeyPatternTypeCons = typeOf[KeyPattern[_]]

    def getTypeScaladoc(tpe: Type): Option[Scaladoc] = {
      import com.github.andyglow.scalamigration._

      val pos = tpe.typeSymbol.pos
      pos.source match {
        case NoSourceFile => None
        case src =>
          val str = new String(src.content, 0, src.lineToOffset(pos.line - 1)).trim
          if (str.endsWith("*/")) {
            val start = str.lastIndexOf("/**")
            if (start >= 0) {
              SlowParser.parse(str.substring(start)).opt
            } else None
          } else None
      }
    }

    def resolveGenericType(x: Type, from: List[Symbol], to: List[Type]): Type = {
      try x.substituteTypes(from, to) catch {
        case NonFatal(_) =>
          c.abort(
            c.enclosingPosition,
            s"""Cannot resolve generic type(s) for `$x`
               |Please provide a custom implicitly accessible json.Schema for it.
               |""".stripMargin)
      }
    }

    sealed trait SealedEnumGen {
      def gen(tpe: Type): Tree
    }
    final object SealedEnumGen {
      final case class FromNames(names: Set[String]) extends SealedEnumGen  {
        def gen(tpe: Type): Tree = {
          val trees = names map { i => q"$intJsonPkg.Value.str($i)" }
          new FromTrees(trees).gen(tpe)
        }
      }
      final case class FromTrees(trees: Set[Tree]) extends SealedEnumGen {
        def gen(tpe: Type): Tree = {
          q"$schemaObj.`enum`[$tpe]($trees)"
        }
      }
    }

    object SealedEnum {

      def unapply(tpe: Type): Option[SealedEnumGen] = {

        if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
          val instances = tpe.typeSymbol.asClass.knownDirectSubclasses
          val toValueTree = c.inferImplicitValue(
            appliedType(toValueTypeCons, tpe),
            silent = true,
            withMacrosDisabled = true)

          if (instances forall { i => val c = i.asClass; c.isModuleClass}) {
            if (toValueTree.nonEmpty) {
              Some(SealedEnumGen.FromTrees(instances collect {
                case i: ClassSymbol =>
                  val caseObj = i.owner.asClass.toType.decls.find { d =>
                    d.name == i.name.toTermName
                  } getOrElse NoSymbol

                  q"$toValueTree($caseObj)"
              }))
            } else {
              Some(SealedEnumGen.FromNames(instances map { i => i.name.decodedName.toString }))
            }
          } else
            None
        } else
          None
      }
    }

    object SealedClasses {
      
      def unapply(tpe: Type): Option[Set[Type]] = {

        def isSealed(x: Type): Boolean = {
          val s = x.typeSymbol
          s.isClass && s.asClass.isSealed
        }

        def isSupportedLeafType(x: Type): Boolean = {
          val s = x.typeSymbol
          s.isClass && !s.isModuleClass && s.asClass.isCaseClass
        }

        // BORROWED:
        // https://github.com/plokhotnyuk/jsoniter-scala/blob/3612fddf19a8ce23ac973d71e85ef02f79c06fff/jsoniter-scala-macros/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L351-L365
        def collectRecursively(x: Type): Seq[Type] =
          if (x.typeSymbol.isClass) {
            val leafs = x.typeSymbol.asClass.knownDirectSubclasses.toSeq flatMap { s =>
              val cs = s.asClass
              val subTpe = if (cs.typeParams.isEmpty) cs.toType else resolveGenericType(cs.toType, cs.typeParams, x.typeArgs)
              if (isSealed(subTpe)) collectRecursively(subTpe)
              else if (isSupportedLeafType(subTpe)) Seq(subTpe)
              else c.abort(c.enclosingPosition, "Only Scala case classes are supported for ADT leaf classes. Please consider using of " +
                s"them for ADT with base '$x' or provide a custom implicitly accessible json.Schema for the ADT base.")
            }
            if (isSupportedLeafType(x)) leafs :+ x else leafs
          } else Seq.empty

        if (isSealed(tpe)) {
          val instances = collectRecursively(tpe)
          Some(instances.toSet)
        } else
          None
      }

      def gen(tpe: Type, subTypes: Set[Type], stack: List[Type]): Tree = {
        val trees = subTypes collect {
          case CaseClass(fields)     => CaseClass.gen(fields, tpe, stack)
          case ValueClass(innerType) => ValueClass.gen(innerType, tpe, stack)
        }

        q"$schemaObj.`oneof`[$tpe]($trees)"
      }
    }

    final class CaseClass(val tpe: Type, val fields: Seq[CaseClass.Field]) {

      def annotations = tpe.typeSymbol.asClass.annotations

      def title: Option[String] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< typeOf[json.schema.title])
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

      def description: Option[String] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< typeOf[json.schema.description])
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

    }

    object CaseClass {

      // TODO: add support for case classes defined in method body

      final def lookupCompanionOf(clazz: Symbol): Symbol = clazz.companion

      def possibleApplyMethodsOf(subjectCompanion: Symbol): List[MethodSymbol] = {
        val subjectCompanionTpe = subjectCompanion.typeSignature

        subjectCompanionTpe.decl(TermName("apply")) match {

          case NoSymbol =>
            c.abort(c.enclosingPosition, s"No apply function found for ${subjectCompanion.fullName}")

          case x => x.asTerm.alternatives flatMap { apply =>
            val method = apply.asMethod

            def areAllImplicit(pss: List[List[Symbol]]): Boolean = pss forall {
              case p :: _ => p.isImplicit
              case _      => false
            }

            method.paramLists match {
              case ps :: pss if ps.nonEmpty && areAllImplicit(pss) => List(method)
              case _ => List.empty
            }
          }
        }
      }

      def applyMethod(subjectCompanion: Symbol): Option[MethodSymbol] =
        possibleApplyMethodsOf(subjectCompanion).headOption

      case class Field(
        name: TermName,
        tpe: Type,
        effectiveTpe: Type,
        annotations: List[Annotation],
        default: Option[Tree],
        isOption: Boolean) {

        def hasDefault: Boolean = default.isDefined
      }

      def fieldMap(tpe: Type): Seq[Field] = {

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

        val annotationMap = annotations0 ++ annotations1

        val subjectCompanionSym = tpe.typeSymbol
        val subjectCompanion    = lookupCompanionOf(subjectCompanionSym)

        def toField(fieldSym: TermSymbol, i: Int): Field = {
          val name        = NameTransformer.decode(fieldSym.name.toString)
          val fieldTpe    = fieldSym.typeSignature.dealias // In(tpe).dealias
          val isOption    = fieldTpe <:< optionTypeCons
          val hasDefault  = fieldSym.isParamWithDefault
          val toV         = c.inferImplicitValue(appliedType(toValueTypeCons, fieldTpe))
          val default     = if (hasDefault) {
            val getter = TermName("apply$default$" + (i + 1))
            if (toV.nonEmpty) Some(q"Some($toV($subjectCompanion.$getter))") else {
              c.error(c.enclosingPosition, s"Can't infer a json value for '$name': $fieldTpe")
              None
            }
          } else
            None

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

// This old approach somehow didn't allow type substitution
// so basically this was not working with generic case classes
//        val fields = applyMethod(subjectCompanion) flatMap { method =>
//          method.paramLists.headOption map { params =>
//            params.map { _.asTerm }.zipWithIndex map { case (f, i) => toField(f, i) }
//          }
//        }
        applyMethod(subjectCompanion)

        val fields = tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption map { params =>
          params.map { _.asTerm }.zipWithIndex map { case (f, i) => toField(f, i) }
        }

        fields getOrElse Seq.empty
      }

      def unapply(tpe: Type): Option[CaseClass] = {
        val symbol = tpe.typeSymbol

        if (symbol.isClass) {
          val clazz = symbol.asClass
          if (clazz.isCaseClass) {
            if (clazz.isDerivedValueClass) None else Some(new CaseClass(tpe, fieldMap(tpe)))
          } else
            None
        } else
          None
      }

      def gen(cc: CaseClass, tpe: Type, stack: List[Type]): Tree = {
        val fieldMap = cc.fields
        // check if all descriptions specified immediately in method call matches field names
        if (stack.isEmpty) descriptions foreach { descriptions =>
          descriptions.keySet foreach { k =>
            if (!fieldMap.exists(_.name.decodedName.toString == k))
              c.abort(c.enclosingPosition, s"unknown field: $k. ${show(tpe)} fields are: ${fieldMap.map(_.name.decodedName.toString).mkString(", ")}")
          }
        }
        val scaladoc = getTypeScaladoc(tpe)
        val objTitle = cc.title
        val objDescr = scaladoc flatMap { _.description } orElse cc.description
        val obj = q"$schemaObj.`object`"
        val fields = fieldMap map { f =>
          val name      = f.name.decodedName.toString
          val jsonType  = resolve(f.effectiveTpe, if (f.isOption) stack else tpe +: stack)
          val fDescr    = {
            def fromScaladoc   = scaladoc flatMap { _.param(name) }
            def fromAnnotation = f.annotations
              .map(_.tree)
              .filter(_.tpe <:< typeOf[json.schema.description])
              .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }
            def fromSpec       = if (stack.isEmpty) descriptions flatMap { _.get(name) } else None

            fromSpec orElse fromScaladoc orElse fromAnnotation
          }

          val tree = f.default map { d =>
            q"$obj.Field.fromJson[${f.effectiveTpe}](name = $name, tpe = $jsonType, required = ${ !f.isOption && !f.hasDefault }, default = $d)"
          } getOrElse {
            q"$obj.Field[${f.effectiveTpe}](name = $name, tpe = $jsonType, required = ${ !f.isOption && !f.hasDefault })"
          }
          fDescr match {
            case Some(descr) => q"$tree.withDescription(Some($descr))"
            case None => tree
          }
        }

        (objDescr, objTitle) match {
          case (Some(descr), Some(title)) => q"$obj[$tpe](..$fields).duplicate(description = Some($descr), title = Some($title))"
          case (_, Some(title))           => q"$obj[$tpe](..$fields).duplicate(title = Some($title))"
          case (Some(descr), _)           => q"$obj[$tpe](..$fields).duplicate(description = Some($descr))"
          case _                          => q"$obj[$tpe](..$fields)"
        }
      }
    }

    object ValueClass {

      def unapply(x: Type): Option[Type] = {
        val symbol = x.typeSymbol

        if (symbol.isClass) {
          val clazz = symbol.asClass
          if (clazz.isCaseClass && clazz.isDerivedValueClass) Some {
              clazz.primaryConstructor.asMethod.paramLists.head.head.typeSignature
          } else
            None
        } else
          None
      }

      def gen(innerType: Type, tpe: Type, stack: List[Type]): Tree = {
        resolve(innerType, tpe +: stack) match {
          case q"""$c[$t](..$args)""" =>
            q"$c[$tpe](..$args)"
          case x =>
            q"""$schemaObj.`value-class`[$tpe, $innerType]($x)"""
//            val st = appliedType(schemaTypeConstructor, tpe)
//            q"$x.asInstanceOf[$st]"
        }
      }
    }

    case class DictionaryGen(tpe: Type, keyType: Type, valueType: Type, keyPattern: Tree) {
      private val stringKey = keyType <:< typeOf[String]

      def gen(stack: List[Type]): Tree = {
        val valueJsonType = resolve(valueType, tpe +: stack)
        val tree = q"""$schemaObj.`dictionary`[$keyType, $valueType, ${tpe.typeConstructor}]($valueJsonType)"""
        val effectiveTree = if (!stringKey) {
          q"""$tree withValidation ($validationObj.patternProperties := $keyPattern)"""
        } else tree

        effectiveTree
      }
    }

    object Dictionary {

      def unapply(x: Type): Option[DictionaryGen] = {
        if (x <:< mapTypeCons) {
          val keyType = x.typeArgs.head
          val valueType = x.typeArgs.tail.head
          c.inferImplicitValue(appliedType(mapKeyPatternTypeCons, keyType)) match {
            case EmptyTree         =>
              keyType match {
                case SealedEnum(SealedEnumGen.FromNames(names)) =>
                  val pattern = names.mkString("^(?:", "|", ")$")
                  Some(DictionaryGen(x, keyType, valueType, q"$pattern"))
                case _                                          => None
              }
            case mapKeyPatternTree => Some(DictionaryGen(x, keyType, valueType, q"$mapKeyPatternTree.pattern"))
          }
        } else {
          None
        }
      }
    }

    object Arr {

      def gen(tpe: Type, stack: List[Type]): Tree = {
        val componentType     = tpe.typeArgs.head
        val componentJsonType = resolve(componentType, tpe +: stack)
        val isSet             = tpe <:< setTypeCons

        if (isSet)
          q"""$schemaObj.`set`[$componentType, ${tpe.typeConstructor}]($componentJsonType)"""
        else
          q"""$schemaObj.`array`[$componentType, ${tpe.typeConstructor}]($componentJsonType)"""
      }
    }

    object Implicit {
      final val LF = '\u000A'
      final val CR = '\u000D'

      sealed trait ImplicitSchema
      case class FromPredef(x: Tree) extends ImplicitSchema
      case class FromSchema(x: Tree) extends ImplicitSchema
      case object NotFound extends ImplicitSchema

      // FIXME: this method is pretty ugly
      //   it is trying to handle situation like this
      //   {{{
      //     case class CC(a: String)
      //     implicit val aSchema: Schema[CC] = Json.schema[CC]
      //   }}}
      //   in this case `inferImplicitValue` returns a self-reference
      //   which turns out to be resolved as `null` eventually
      //
      //   So this method take a line of a source code where our def-macro is used and
      //   see if it is assignment and it assigns to variable returned by `inferImplicitValue`
      //
      def isSelfRef(x: Tree): Boolean = x match {
        case x @ Select(This(_), TermName(field)) =>
          val pos = x.pos
          pos.source match {
            case NoSourceFile => false
            case src =>
              val end = src.lineToOffset(src.offsetToLine(pos.point))
              var start = end - 2

              while ({
                !src.isLineBreak(start) &&
                  !src.isEndOfLine(start)
              }) start = start - 1

              val str = new String(src.content, start, pos.point - start)
              if (str.contains(field) && str.contains("=") && str.contains("implicit")) true
              else false
          }
        case _ => false
      }


      def getOrElse(tpe: Type, gen: => Tree): Tree = {
        // def debug(msg: String): Unit = c.info(c.enclosingPosition, msg, force = true)

        val sType = appliedType(schemaTypeConstructor, tpe) // .dealias.widen
        val pType = appliedType(predefTypeConstructor, tpe) // .dealias.widen

        // search implicit scope for a schema
        //
        // this 2 level implicit search is done this way because Schema/Predef
        // are covariants which makes impossible use of implicit conversions like
        // ```
        // def schemaFromPredef[T](implicit p: Predef[T]): Schema[T] = p.schema
        // ```
        // if taken from schema - must be exposed as `ref`
        // if taken from predef - exposing as is
        def lookupSchema: ImplicitSchema = {
          // schema
          c.inferImplicitValue(sType) match {
            case EmptyTree =>
              // predef
              c.inferImplicitValue(pType) match {
                case EmptyTree  => NotFound
                case x          => FromPredef(q"$x.schema")
              }
            case x if isSelfRef(x) => NotFound
            case x => FromSchema(x)
          }
        }

        lookupSchema match {
          case NotFound      => gen
          case FromPredef(x) => x
          case FromSchema(x) => q"""$schemaObj.`ref`[$tpe]($x)($jsonPkg.Json.sig[$tpe].signature)"""
        }
      }
    }

    def resolve(tpe: Type, stack: List[Type]): Tree = {
      if (stack contains tpe) c.error(c.enclosingPosition, s"cyclic dependency for $tpe")

      def genTree: Tree = tpe match {
        case Dictionary(g)                      => g.gen(stack)
        case x if x <:< typeOf[Array[_]]        => Arr.gen(x, stack)
        case x if x <:< typeOf[Iterable[_]]     => Arr.gen(x, stack)
        case SealedEnum(g)                      => g.gen(tpe)
        case SealedClasses(subTypes)            => SealedClasses.gen(tpe, subTypes, stack)
        case CaseClass(fields)                  => CaseClass.gen(fields, tpe, stack)
        case ValueClass(innerType)              => ValueClass.gen(innerType, tpe, stack)

        case _ =>
          c.error(c.enclosingPosition, s"schema for $tpe is not supported, ${stack mkString " :: "}")
          q"null"
      }

      Implicit.getOrElse(tpe, genTree)
    }

    val out = resolve(subject, Nil)

    if (c.settings.contains("print-jsonschema-code"))
     c.info(c.enclosingPosition, show(out), force = false)

    c.Expr[S[T]](out)
  }
}
