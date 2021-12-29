package com.github.andyglow.jsonschema

private[jsonschema] trait UArrays { this: UContext with UCommons =>
  import c.universe._

  class ArrExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Arr] = {
      Some.when(tpe <:< T.array || tpe <:< T.iterable) {
        val containerTpe  = tpe.typeConstructor
        val elementTpe    = tpe.typeArgs.head
        val elementSchema = resolve(elementTpe, ctx :+ tpe)

        U.Arr(elementTpe, containerTpe, elementSchema, unique = tpe <:< T.set)
      }
    }
  }

  val Arr = new ArrExtractor
}
