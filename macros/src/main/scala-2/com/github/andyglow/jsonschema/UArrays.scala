package com.github.andyglow.jsonschema

private[jsonschema] trait UArrays { this: UContext with UCommons =>
  import c.universe._

  class ArrExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] =
      // (`Null` conforms too, but has no base type: left to the Scala branches, as before)
      if (tpe <:< T.jCollection && tpe.baseType(T.jCollection.typeSymbol).typeArgs.nonEmpty) Some(javaCollection(tpe)) else scalaCollection(tpe)

    // a `java.util.Collection[E]` (List, Set, ArrayList, EnumSet, ...): built with a canonical container, `java.util.Set` or `java.util.Collection`, as a `Schema[tpe]`.
    // Its own constructor can't be the `C[_]` of `array[E, C]` (`EnumSet[E <: Enum[E]]` has a bounded parameter), and it may take no element parameter at all
    private def javaCollection(tpe: Type)(implicit ctx: ResolutionContext): SchemaType = {
      val elementTpe = tpe.baseType(T.jCollection.typeSymbol).typeArgs.head
      val unique     = tpe <:< T.jSet
      val container  = (if (unique) T.jSet else T.jCollection).typeConstructor
      U.Cast(tpe, U.Arr(elementTpe, container, resolve(elementTpe, ctx :+ tpe), unique))
    }

    private def scalaCollection(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Arr] = {
      Some.when(tpe <:< T.array || tpe <:< T.iterable) {
        val containerTpe = tpe.typeConstructor
        val elementTpe   = (if (tpe <:< T.array) tpe else tpe.baseType(T.iterable.typeSymbol)).typeArgs.head
        // the generated `array[E, C]` needs a container C[_] whose only type parameter is the element type
        // (not so for e.g. BitSet, or a `class Path[A] extends Iterable[String]`)
        if (!tpe.typeArgs.headOption.exists(_ =:= elementTpe) || tpe.typeArgs.size != 1)
          notSupported(
            tpe,
            ctx.stack,
            s"a collection must take its element type as its only type parameter (like List[T] or Set[T]), and ${showType(tpe)} is a collection of ${showType(elementTpe)}. $ProvideHint"
          )
        val elementSchema = resolve(elementTpe, ctx :+ tpe)

        U.Arr(elementTpe, containerTpe, elementSchema, unique = tpe <:< T.set)
      }
    }
  }

  val Arr = new ArrExtractor
}
