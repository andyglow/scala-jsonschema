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
    with UScalaParsers {

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

  /** Derives a Predef
    *
    * @tparam T
    * @return
    */
  def derivePredef[T: c.WeakTypeTag]: c.Expr[json.schema.Predef[T]] =
    c.Expr[json.schema.Predef[T]] {
      val schema = deriveInternal[T, json.Schema]()
      q"${N.Predef}($schema)"
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
      deriveInternal[T, json.Schema.`object`](specFD)
    }
  }

  protected def deriveInternal[T: c.WeakTypeTag, S[_]](
    specFD: FieldDecorations = FieldDecorations.Empty,
    noImplicitSearch: Boolean = false
  ): c.Expr[S[T]] = {
    val tpe = weakTypeOf[T]

    val typeDeco = TypeAnnotations(tpe)

    val recursiveTypes = new RecursiveTypes
    implicit val ctx   = new ResolutionContext(Nil, recursiveTypes.append)

    val out = {

      val st = recursiveTypes.substitute(
        resolve(tpe, ctx, specFD, noImplicitSearch)
      )

      st.withExtra(
        st.extra.copy(
          title = typeDeco.texts.flatMap(_.title),
          description = typeDeco.texts.flatMap(_.description)
        )
      )

    }.tree

    // debug
    if (c.settings.contains("print-jsonschema-code") || debugEnabled)
      c.info(c.enclosingPosition, showCode(out), force = false)

    c.Expr[S[T]](out)
  }

  def resolve(
    tpe: Type,
    ctx: ResolutionContext,
    specFD: FieldDecorations = FieldDecorations.Empty,
    noImplicitSearch: Boolean = false
  ): SchemaType = {
    if (ctx contains tpe) {
      val sig = signature(tpe)
      ctx.onCycle(tpe)
      U.Ref(tpe, q"$sig")
    } else if (T.isNothing(tpe)) {
      c.abort(c.enclosingPosition, "No json.Schema[_] for Nothing")
    } else {
      implicit def _ctx    = ctx
      implicit def _specFD = specFD

      def genTree: SchemaType = tpe match {
        case Dict(x)       => x
        case Arr(x)        => x
        case Enum(x)       => x
        case SumType(x)    => x
        case CaseClass(x)  => x
        case ValueClass(x) => x
        case _ =>
          c.abort(
            c.enclosingPosition,
            s"schema for $tpe is not supported, ${ctx.stack mkString " :: "}"
          )
      }

      if (noImplicitSearch) genTree else Implicit.getOrElse(tpe, genTree)
    }
  }
}

class Macroses(val c: blackbox.Context) extends MacroCake
