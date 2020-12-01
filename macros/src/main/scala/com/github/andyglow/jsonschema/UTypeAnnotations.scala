package com.github.andyglow.jsonschema

import com.github.andyglow.scaladoc.Scaladoc


private[jsonschema] trait UTypeAnnotations { this: UContext with UCommons with UScaladocs with USignatures =>
  import c.universe._

  final case class Texts(
    title: Option[String],
    description: Option[String]) {

    def isEmpty: Boolean = title.isEmpty && description.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def orElse(x: Texts): Texts = Texts(
      title = title orElse x.title,
      description = description orElse x.description)
  }

  final object Texts {

    def fromScaladoc(scaladoc: Scaladoc): Texts = {
      Texts(None, scaladoc.description)
    }

    def fromAnnotations(tpe: Type): Texts = {
      val annotations = tpe.typeSymbol.asClass.annotations

      val title: Option[String] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.title)
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

      val description: Option[String] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.description)
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

      Texts(title, description)
    }

    def apply(tpe: Type): Texts = {
      val fromSD = getTypeScaladoc(tpe) map fromScaladoc
      def fromAN = fromAnnotations(tpe)

      fromSD.fold(fromAN){ _ orElse fromAN }
    }
  }

  sealed trait DefinitionKey
  final object DefinitionKey {
    final case object Infer extends DefinitionKey
    final case class Defined(value: String) extends DefinitionKey
  }

  final case class Discriminator(field: String, phantom: Boolean)

  final case class DiscriminatorKey(value: String)

  case class TypeAnnotations(
    texts: Option[Texts],
    definition: Option[DefinitionKey],
    discriminator: Option[Discriminator],
    discriminatorKey: Option[DiscriminatorKey]) {

    def wrapIntoDefIfRequired(tpe: Type, schema: SchemaType): SchemaType = {
      import SchemaType._

      definition.fold(schema) {
        case DefinitionKey.Infer =>
          schema match {
            case d: Def => d
            case _      => Def(tpe, q"${signature(tpe)}", schema)
          }
        case DefinitionKey.Defined(newSig) =>
          schema match {
            case d: Def => d.copy(sig = q"$newSig")
            case _      => Def(tpe, q"$newSig", schema)
          }
      }
    }
  }

  final object TypeAnnotations {

    val Empty = TypeAnnotations(None, None, None, None)

    def fromAnnotations(tpe: Type): TypeAnnotations = {
      val annotations = tpe.typeSymbol.asClass.annotations

      val texts = Texts(tpe).filter(_.nonEmpty)

      val definition: Option[DefinitionKey] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.definition)
          .collectFirst {
            case Apply(_, List(Select(_, TermName("$lessinit$greater$default$1")))) => DefinitionKey.Infer
            case Apply(_, List(Literal(Constant(text: String))))                    => DefinitionKey.Defined(text)
          }

      val discriminator: Option[Discriminator] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.discriminator)
//          .map { x => c.info(c.enclosingPosition, "BipBip: \n" + showRaw(x) + "\n" + show(x), force = true); x }
          .collectFirst {
            case Apply(_, fieldTree :: phantomTree :: Nil) =>
              val field = fieldTree match {
                case Literal(Constant(text: String))                    => text
                case Select(_, TermName("$lessinit$greater$default$1")) => "type"
                case _                                                  => c.abort(c.enclosingPosition, "@discriminator annotation: please even if using call-by-name, use it in specified order. aka [field, phantom]")
              }
              val phantom = phantomTree match {
                case Literal(Constant(v: Boolean))                      => v
                case Select(_, TermName("$lessinit$greater$default$2")) => true
                case _                                                  => c.abort(c.enclosingPosition, "@discriminator annotation: please even if using call-by-name, use it in specified order. aka [field, phantom]")
              }

              Discriminator(field, phantom)
          }

      val discriminatorKey: Option[DiscriminatorKey] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.discriminatorKey)
          .collectFirst {
            case Apply(_, List(Literal(Constant(text: String)))) => DiscriminatorKey(text)
          }

      TypeAnnotations(texts, definition, discriminator, discriminatorKey)
    }

    def apply(tpe: Type): TypeAnnotations = fromAnnotations(tpe)
  }
}
