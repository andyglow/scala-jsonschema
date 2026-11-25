package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

trait MacroCake
    extends UContext
    with UCommons
    with UFlags
    with UImplicits
    with USignatures
    with UScaladocs
    with UArrays
    with UDictionaries
    with UEnums
    with URecursiveTypes
    with UValueTypes
    with UProductTypes
    with USumTypes
    with UTypeAnnotations
    with UJsonValueType
    with UFieldDecorations
    with UScalaParsers
    with USemiAuto {

  import c.universe._

  /** Type Signature
    *
    * @tparam T
    * @return
    */
  def deriveSignature[T: c.WeakTypeTag]: c.Expr[TypeSignature[T]] = {
    val tpe = weakTypeOf[T]
    val sig = signature(tpe)

    c.Expr[TypeSignature[T]] {
      q"${N.internal.TypeSignature}[$tpe]($sig)"
    }
  }

  /** `Json.auto.derived[T]` (issue #542). Inside a derivation's implicit search: the placeholder `Exported(AutoHole.hole[T])`, which that derivation fills in from its own expansion. Anywhere else:
    * `T` derived deeply, in auto mode, in one expansion.
    */
  def deriveAuto[T: c.WeakTypeTag]: c.Expr[json.schema.Exported[json.schema.Predef[T]]] = {
    val tpe       = weakTypeOf[T]
    val predefTpe = tq"_root_.json.schema.Predef[$tpe]"
    c.Expr[json.schema.Exported[json.schema.Predef[T]]] {
      if (AutoSearch.isSearching) q"_root_.json.schema.Exported[$predefTpe](_root_.com.github.andyglow.jsonschema.internal.AutoHole.hole[$tpe])"
      else {
        // An abort here, while a search for a `Predef` tries the bridge `importedPredef` (which takes this as its implicit argument, expanded at once), makes the bridge fail: the user's
        // search reports "could not find implicit value", without the reason (Scala 2's searches keep no reason; design S2-R6).
        autoRoot = true
        val schema = deriveInternal[T, json.Schema]()
        q"_root_.json.schema.Exported[$predefTpe](${N.Predef}[$tpe]($schema))"
      }
    }
  }

  /** Derives a Schema
    *
    * @tparam T
    * @return
    */
  def deriveSchema[T: c.WeakTypeTag]: c.Expr[json.Schema[T]] = deriveInternal[T, json.Schema]()

  /** Derives an Object Schema
    *
    * @tparam T
    * @return
    */
  def deriveObjectSchema[T: c.WeakTypeTag](
    decorations: c.Expr[(String, String)]*
  ): c.Expr[json.Schema.`object`[T]] = {
    val tpe = weakTypeOf[T]
    validateNonValueCaseClass(tpe, "Json.objectSchema") {
      val specFD = FieldDecorations.fromSpec(decorations)
      deriveInternal[T, json.Schema.`object`](specFD, requireObject = true)
    }
  }

  protected def deriveInternal[T: c.WeakTypeTag, S[_]](
    specFD: FieldDecorations = FieldDecorations.Empty,
    noImplicitSearch: Boolean = false,
    requireObject: Boolean = false
  ): c.Expr[S[T]] = {
    val tpe = weakTypeOf[T]

    val typeDeco = TypeAnnotations(tpe)

    val recursiveTypes = new RecursiveTypes
    implicit val ctx   = new ResolutionContext(Nil, recursiveTypes.append)

    var rootFromImplicit = false
    val out0             = {

      val st = recursiveTypes.substitute(
        resolve(tpe, ctx, specFD, noImplicitSearch)
      )
      rootFromImplicit = st.isInstanceOf[U.`-from-tree-`]

      if (requireObject && !st.isInstanceOf[U.Obj])
        c.abort(
          c.enclosingPosition,
          s"Json.objectSchema[$tpe]: $tpe doesn't derive to an object schema (an implicit json.Schema[$tpe] or json.schema.Predef[$tpe] is in scope, or $tpe is recursive). Use Json.schema[$tpe]"
        )

      st.withExtra(
        st.extra.copy(
          title = typeDeco.texts.flatMap(_.title),
          description = typeDeco.texts.flatMap(_.description),
          deprecated = st.extra.deprecated || typeDeco.deprecated
        )
      )

    }.tree

    Implicit.leftoverHole(out0) foreach { h =>
      c.abort(
        c.enclosingPosition,
        s"json-schema: an auto-derivation placeholder was left in the derived schema of ${showType(tpe)}: ${showCode(h)} (a bug in the derivation, please report it)"
      )
    }

    val out = autoMarker(buildingRoot(tpe, out0, rootFromImplicit, requireObject))

    // debug
    if (c.settings.contains("print-jsonschema-code") || debugEnabled)
      c.info(c.enclosingPosition, showCode(out), force = false)

    c.Expr[S[T]](out)
  }

  def resolve(
    tpe0: Type,
    ctx: ResolutionContext,
    specFD: FieldDecorations = FieldDecorations.Empty,
    noImplicitSearch: Boolean = false
  ): SchemaType = {
    // see through a type alias (`type IntBox = Box[Int]`, `type Users = List[User]`): the alias has no type arguments of its own, and refs and defs must be named after the same type
    val tpe  = tpe0.dealias
    val seen = ctx find tpe
    if (seen.isDefined) {
      // a recursive ref takes the name of the def it closes: `dealias` doesn't normalize type arguments, so the current spelling may differ
      val defTpe = seen.get
      val sig    = signature(defTpe)
      ctx.onCycle(defTpe)
      U.Ref(defTpe, q"$sig")
    } else if (T.isNothing(tpe)) {
      c.abort(c.enclosingPosition, "No json.Schema[_] for Nothing" + pathOf(ctx.stack))
    } else {
      implicit def _ctx    = ctx
      implicit def _specFD = specFD

      def genTree(auto: Boolean): SchemaType = {
        if (!auto && !ctx.isEmpty && isGatedKind(tpe) && !isCaseOfFamilyOnStack(tpe, ctx.stack)) semiAutoGate(tpe, ctx.stack)
        structural
      }

      def structural: SchemaType = tpe match {
        // `Null <: Map`, `Null <: Iterable`: before the families, which would claim it (after the implicit search: a user `Predef[Null]` wins)
        case t if t.typeSymbol == definitions.NullClass => notSupported(t, ctx.stack, NullReason)
        // `Option[X]` in every position but a field (UProductTypes wraps those): before the sum types, which would derive it as `Some | None`
        case t if t.typeSymbol == T.option.typeSymbol && t.typeArgs.size == 1 =>
          val x = t.typeArgs.head
          U.Opt(t, x, resolve(x, ctx :+ t))
        // `java.util.Optional[X]`: the same `option` node, as a `Schema[Optional[X]]`
        case t if t.typeSymbol == T.jOptional.typeSymbol && t.typeArgs.size == 1 =>
          val x = t.typeArgs.head
          U.Cast(t, U.Opt(appliedType(T.option.typeConstructor, List(x)), x, resolve(x, ctx :+ t)))
        case Dict(x)       => x
        case Arr(x)        => x
        case Enum(x)       => x
        case SumType(x)    => x
        case CaseClass(x)  => x
        case ValueClass(x) => x
        case _             => notSupported(tpe, ctx.stack, singletonReason(tpe) getOrElse "")
      }

      if (noImplicitSearch) genTree(autoRoot)
      else
        Implicit.lookup(tpe) match {
          case Implicit.Lookup.Found(st)     => st
          case Implicit.Lookup.AutoAvailable => genTree(auto = true)
          case Implicit.Lookup.Missing       => genTree(autoRoot)
        }
    }
  }
}

class Macroses(val c: blackbox.Context) extends MacroCake
