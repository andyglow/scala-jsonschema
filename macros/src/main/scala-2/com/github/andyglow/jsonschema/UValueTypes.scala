package com.github.andyglow.jsonschema

private[jsonschema] trait UValueTypes { this: UContext with UCommons =>
  import c.universe._

  class ValueClassExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.ValueClass] = {
      val symbol = tpe.typeSymbol

      Option.whenever(symbol.isClass) {
        val clazz = symbol.asClass
        Some.when(clazz.isCaseClass && clazz.isDerivedValueClass) {
          val declared  = clazz.primaryConstructor.asMethod.paramLists.head.head.typeSignature
          // `case class Id[T](v: T) extends AnyVal` as Id[Int]: the inner type is Int, not T
          val innerType = if (tpe.typeArgs.nonEmpty) resolveGenericType(declared, clazz.typeParams, tpe.typeArgs) else declared
          U.ValueClass(tpe, innerType, resolve(innerType, ctx :+ tpe))
        }
      }
    }
  }

  val ValueClass = new ValueClassExtractor
}
