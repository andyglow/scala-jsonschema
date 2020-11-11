package com.github.andyglow.jsonschema

import com.github.andyglow.scaladoc.Scaladoc


private[jsonschema] trait UTypeDecorations { this: UContext with UCommons with UScaladocs =>
  import c.universe._

  case class TypeDecoration(title: Option[String], description: Option[String]) {

    def orElse(x: TypeDecoration): TypeDecoration = TypeDecoration(
      title = title orElse x.title,
      description = description orElse x.description)
  }

  final object TypeDecoration {

    val Empty = TypeDecoration(None, None)

    def fromScaladoc(scaladoc: Scaladoc): TypeDecoration = {
      TypeDecoration(None, scaladoc.description)
    }

    def fromAnnotations(tpe: Type): TypeDecoration = {
      val annotations = tpe.typeSymbol.asClass.annotations

      val title: Option[String] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.decoration.title)
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

      val description: Option[String] =
        annotations
          .map(_.tree)
          .filter(_.tpe <:< T.decoration.description)
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

      TypeDecoration(title, description)
    }

    def apply(tpe: Type): TypeDecoration = {
      val fromSD = getTypeScaladoc(tpe) map fromScaladoc
      val fromAN = fromAnnotations(tpe)

      fromSD.fold(fromAN){ _ orElse fromAN }
    }
  }
}
