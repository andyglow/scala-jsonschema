package com.github.andyglow.jsonschema

private[jsonschema] trait UArrays { this: UContext with UCommons =>
  import c.universe._

  class ArrExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Arr] = {
      Some.when(tpe <:< T.array || tpe <:< T.iterable) {
        val containerTpe = tpe.typeConstructor
        val elementTpe   = (if (tpe <:< T.array) tpe else tpe.baseType(T.iterable.typeSymbol)).typeArgs.head
        // the generated `array[E, C]` needs a container C[_] whose only type parameter is the element type
        // (not so for e.g. BitSet, or a `class Path[A] extends Iterable[String]`)
        if (!tpe.typeArgs.headOption.exists(_ =:= elementTpe) || tpe.typeArgs.size != 1)
          c.abort(
            c.enclosingPosition,
            s"schema for $tpe is not supported: a collection must take its element type as its only type parameter (like List[T] or Set[T]), and $tpe is a collection of $elementTpe. " +
              s"Provide a json.schema.Predef or json.Schema for it"
          )
        val elementSchema = resolve(elementTpe, ctx :+ tpe)

        U.Arr(elementTpe, containerTpe, elementSchema, unique = tpe <:< T.set)
      }
    }
  }

  val Arr = new ArrExtractor
}
