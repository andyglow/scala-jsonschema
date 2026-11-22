package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.jsonschema.TypeSignature
import json.Schema
import json.schema.Predef

/** One macro expansion. A cake: every trait is one concern in its own file, and they all share `q` (`import q.reflect.*` in each). See `notes/reports/skeleton-ir.md`.
  *
  * Passes: [[Resolve.resolve]] builds the IR ([[Nodes]]) -> [[Recursion.substituteRecursive]] -> the root's type texts -> [[Emit.emit]].
  *
  * @param objectSchemaDecorations
  *   `Json.objectSchema` field descriptions (root only)
  */
final class Derivation(val mode: Mode, val objectSchemaDecorations: List[(String, String)])(implicit val q: Quotes)
    extends Nodes
    with Resolve
    with Implicits
    with Recursion
    with Signatures
    with Annotations
    with Emit
    with Options
    with Products
    with ValueClasses
    with Sums
    with Enums
    with Collections {
  import q.reflect.*

  /** The IR of `T`, after the IR -> IR passes. */
  def build[T: Type]: Node = {
    val tpe  = TypeRepr.of[T]
    val node = substituteRecursive(resolve(tpe, Ctx.root))
    // Scala 2 `deriveInternal`: the root's title and description come from its type's annotations/scaladoc, replacing whatever the node had (R8: nested types don't get them).
    // That applies to a root from an implicit Schema/Predef too (lead decision 2026-11-20: parity). There, the scaladoc counts only for a type compiled in this run, which is all Scala 2
    // can see: with -Yread-docs a library type's own scaladoc (cats' NonEmptyList) would otherwise become the description of Json.schema[NonEmptyList[String]].
    val ta = node match {
      case _: FromImplicit => typeAnnotations(tpe, docsFromClasspath = false)
      case _               => typeAnnotations(tpe)
    }
    val root = node.withExtra(node.extra.copy(title = ta.title, description = ta.description))
    if (mode == Mode.Derived) derivedDefinition(tpe.dealias, root) else root
  }

  /** `Schema.derived[X]` (`derives json.Schema`) is the Scala 3 spelling of a user-provided implicit `Schema[X]` in the companion, so it is a definition named after `X` (lead decision 2026-11-20,
    * reviewer3 B1 option (b)): `summon[Schema[X]]` returns `def(sig(X), ..)`, and a generic class's `derives` given passes it on as is (see [[Nodes.FromImplicit]]). Exceptions:
    *   - a root that is already a definition (a recursive type) isn't wrapped again;
    *   - a root that mentions an abstract type (the given of a generic `derives`, `GBox[T]`) is inlined, so `GBox[Int]` and `GBox[String]` never share a name. A consumer that finds the given for a
    *     concrete `GBox[Int]` names the definition itself, like for any user implicit.
    */
  private def derivedDefinition(tpe: TypeRepr, root: Node): Node = root match {
    case _: Def                         => root
    case _ if mentionsAbstractType(tpe) => root
    case _                              => Def(tpe, signature(tpe, dealiased = true), root)
  }

  def run[T: Type]: Expr[Schema[T]] = {
    val out = emit(build[T])
    Derivation.debug(out)
    out match {
      case '{ $s: Schema[T] } => s
      case _                  => '{ $out.asInstanceOf[Schema[T]] }
    }
  }

  def signatureOf[T: Type]: String = signature(TypeRepr.of[T])
}

object Derivation {

  def derive[T: Type](mode: Mode)(using Quotes): Expr[Schema[T]] =
    new Derivation(mode, Nil).run[T]

  def deriveAuto[T: Type](using Quotes): Expr[Predef[T]] =
    if (AutoGuard.isSearching) AutoGuard.decline // a derivation is searching: see AutoGuard
    else {
      val s = derive[T](Mode.Auto)
      '{ Predef[T]($s) }
    }

  def signature[T: Type](using Quotes): Expr[TypeSignature[T]] = {
    val sig = Expr(new Derivation(Mode.Schema, Nil).signatureOf[T])
    '{ TypeSignature[T]($sig) }
  }

  def deriveObject[T: Type](decorations: Expr[Seq[(String, String)]])(using Quotes): Expr[Schema.`object`[T]] = {
    import quotes.reflect.*
    val ObjectSchema = "Json.objectSchema"
    val sym          = TypeRepr.of[T].dealias.typeSymbol
    if (!sym.isClassDef) report.errorAndAbort(s"$ObjectSchema: can be used only against case classes")
    // a case object passes (Scala 2: its module class is a case class) and fails in the derivation: "is not supported"
    else if (!sym.flags.is(Flags.Case)) report.errorAndAbort(s"$ObjectSchema: can't be used against non-case classes")
    else if (TypeRepr.of[T] <:< TypeRepr.of[AnyVal]) report.errorAndAbort(s"$ObjectSchema: can't be used against value classes")

    val d    = new Derivation(Mode.ObjectSchema, decorationsOf(decorations))
    val node = d.build[T]
    node match {
      case _: d.Obj => ()
      case _        =>
        val t = Type.show[T]
        report.errorAndAbort(s"$ObjectSchema[$t]: $t is a definition, not an object schema (an implicit json.Schema[$t] is in scope, or $t is recursive). Use Json.schema[$t]")
    }
    val out = d.emit(node)
    debug(out)
    out match {
      case '{ $s: Schema.`object`[T] } => s
      case _                           => '{ $out.asInstanceOf[Schema.`object`[T]] }
    }
  }

  private val ConstantStrings = "only constant Strings are allowed in Description specification"

  /** `"field" -> "description"` / `("field", "description")` pairs; literals only (Scala 2 parity). */
  private def decorationsOf(decorations: Expr[Seq[(String, String)]])(using Quotes): List[(String, String)] = {
    import quotes.reflect.*
    def literal(e: Expr[String]): String = e.value getOrElse report.errorAndAbort(s"$ConstantStrings, got ${e.show}", e)
    decorations match {
      case Varargs(pairs) =>
        pairs.toList map {
          case '{ ($k: String) -> ($v: String) } => (literal(k), literal(v))
          case '{ ($k: String, $v: String) }     => (literal(k), literal(v))
          case other                             => report.errorAndAbort(s"$ConstantStrings, got ${other.show}", other)
        }
      case other => report.errorAndAbort(s"$ConstantStrings: pass the decorations as literal pairs, got ${other.show}", other)
    }
  }

  /** `sbt -Djsonschema.print-code=true` prints every generated tree (Scala 2: `-Xmacro-settings:print-jsonschema-code`). */
  private def debug(out: Expr[?])(using Quotes): Unit =
    if (sys.props.get("jsonschema.print-code").contains("true")) quotes.reflect.report.info(out.show)
}
