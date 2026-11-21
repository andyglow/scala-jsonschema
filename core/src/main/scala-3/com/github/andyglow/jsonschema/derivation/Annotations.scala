package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import _root_.scaladoc.{Scaladoc, Tag}
import _root_.scaladoc.parser.ScaladocParser

/** An annotation argument as the user wrote it. Kept apart from the value so emit can tell "user-supplied" from "the annotation's default" (profiledesign fact 3). */
enum Arg[+A] {

  /** A literal the user wrote. */
  case Given(value: A)

  /** The argument was left out: the annotation's default applies. */
  case Default

  /** Something other than a literal (ignored, like Scala 2 does). */
  case NonLiteral

  def toOption: Option[A] = this match {
    case Given(v) => Some(v)
    case _        => None
  }
}

/** Annotations and scaladoc: shared readers for type-level texts (root, sum members) and annotation arguments.
  *
  * Type level (Scala 2's `UTypeAnnotations`): the title comes from `@title`; the description from the scaladoc's description, else `@description`. A scaladoc without description text (only `@param`s)
  * counts as absent (TRIAGE-B). Field-level decorations (`@description` on params, scaladoc `@param`, objectSchema config) belong to Products (stream 1).
  *
  * Scaladoc: `Symbol.docstring` reads docs from source in the same run, and from TASTy (other modules, jars, files not recompiled) only if the *consumer* compiles with `-Yread-docs`.
  */
trait Annotations { this: Derivation =>
  import q.reflect.*

  /** What a type's annotations and scaladoc say. Every family reads what it needs; nothing here is applied automatically except the root's texts. */
  final case class TypeAnnotations(
    title: Option[String],
    description: Option[String],
    definition: Option[Arg[String]],
    discriminator: Option[TypeAnnotations.Discriminator],
    discriminatorKey: Option[String],
    typeHint: Option[TypeRepr]
  ) {
    def texts: Extra = Extra(title = title, description = description)
  }
  object TypeAnnotations {
    final case class Discriminator(field: Arg[String], phantom: Arg[Boolean])
  }

  /** @param docsFromClasspath
    *   read the scaladoc of a type that isn't compiled in this run too (from TASTy, with `-Yread-docs`)
    */
  def typeAnnotations(tpe: TypeRepr, docsFromClasspath: Boolean = true): TypeAnnotations = {
    val sym            = classSymbol(tpe)
    val annots         = annotationsOf(sym)
    val fromAnnotation = findAnnotation[json.schema.description](annots).flatMap(a => stringArg(a, "text", 0).toOption)
    val fromScaladoc   = if (docsFromClasspath || sym.isDefinedInCurrentRun) scaladocOf(sym).flatMap(docDescription) else None
    TypeAnnotations(
      title = findAnnotation[json.schema.title](annots).flatMap(a => stringArg(a, "text", 0).toOption),
      description = fromScaladoc orElse fromAnnotation,
      definition = findAnnotation[json.schema.definition](annots).map { a =>
        stringArg(a, "label", 0) match {
          case Arg.Given("__default__") => Arg.Default // Scala 2: `@definition("__default__")` = no label
          case other                    => other
        }
      },
      discriminator = findAnnotation[json.schema.discriminator](annots).map { a =>
        TypeAnnotations.Discriminator(stringArg(a, "field", 0), boolArg(a, "phantom", 1))
      },
      discriminatorKey = findAnnotation[json.schema.discriminatorKey](annots).flatMap(a => stringArg(a, "label", 0).toOption),
      typeHint = findAnnotation[json.schema.typeHint[?]](annots).flatMap(_.tpe.typeArgs.headOption)
    )
  }

  // ---- annotations

  def annotationsOf(sym: Symbol): List[Term] = if (sym.isNoSymbol) Nil else sym.annotations

  def findAnnotation[A: Type](annots: List[Term]): Option[Term] = annots.find(_.tpe <:< TypeRepr.of[A])

  def hasAnnotation[A: Type](annots: List[Term]): Boolean = findAnnotation[A](annots).isDefined

  /** The argument `name` (at position `index`) of an annotation tree `new A(...)`. */
  def argOf(annotation: Term, name: String, index: Int): Arg[Term] = {
    val args = annotation match {
      case Apply(_, as) => as
      case _            => Nil
    }
    val named = args.collectFirst { case NamedArg(`name`, v) => v }
    val found = named orElse args.lift(index).map {
      case NamedArg(_, v) => v
      case v              => v
    }
    found match {
      case None                       => Arg.Default
      case Some(t) if isDefaultArg(t) => Arg.Default
      case Some(t)                    => Arg.Given(t)
    }
  }

  def stringArg(annotation: Term, name: String, index: Int): Arg[String] = argOf(annotation, name, index) match {
    case Arg.Given(t) =>
      literal(t) match {
        case Some(StringConstant(s)) => Arg.Given(s)
        case _                       => Arg.NonLiteral
      }
    case Arg.Default    => Arg.Default
    case Arg.NonLiteral => Arg.NonLiteral
  }

  def boolArg(annotation: Term, name: String, index: Int): Arg[Boolean] = argOf(annotation, name, index) match {
    case Arg.Given(t) =>
      literal(t) match {
        case Some(BooleanConstant(b)) => Arg.Given(b)
        case _                        => Arg.NonLiteral
      }
    case Arg.Default    => Arg.Default
    case Arg.NonLiteral => Arg.NonLiteral
  }

  private def literal(t: Term): Option[Constant] = t match {
    case Literal(c)         => Some(c)
    case Inlined(_, Nil, e) => literal(e)
    case Typed(e, _)        => literal(e)
    case _                  => None
  }

  /** A left-out argument shows up as a call of the annotation's default getter, `<annotation>.$lessinit$greater$default$N`. */
  private def isDefaultArg(t: Term): Boolean = t match {
    case Select(_, n)       => n.contains("$default$")
    case Ident(n)           => n.contains("$default$")
    case Inlined(_, Nil, e) => isDefaultArg(e)
    case _                  => false
  }

  // ---- scaladoc

  /** Where to report a problem with `sym`'s scaladoc: the symbol itself if it is compiled in this run; a symbol loaded from a jar has no position (asking warns "Missing symbol position"). */
  private def docPosition(sym: Symbol): Position =
    if (sym.isDefinedInCurrentRun) sym.pos.getOrElse(Position.ofMacroExpansion) else Position.ofMacroExpansion

  /** The parsed scaladoc of `sym`. A doc the parser rejects is reported as a warning and ignored. */
  def scaladocOf(sym: Symbol): Option[Scaladoc] =
    if (sym.isNoSymbol) None
    else
      sym.docstring flatMap { raw =>
        ScaladocParser.fromDocstring(raw) match {
          case Right(doc) => Some(doc)
          case Left(err)  =>
            report.warning(s"can't parse the scaladoc of ${sym.fullName}, ignoring it: ${err.getMessage}", docPosition(sym))
            None
        }
      }

  /** The description part of a scaladoc (Scala 2's `Texts.fromScaladoc`), or `None` if it has no text. */
  def docDescription(doc: Scaladoc): Option[String] = {
    val text = doc.tags.collect { case Tag.Description(d) => d.trimmed.plainString }.mkString("\n")
    Option.when(text.nonEmpty)(text)
  }

  /** `@param` texts by parameter name. */
  def docParams(doc: Scaladoc): Map[String, String] = doc.textParams
}
