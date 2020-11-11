package com.github.andyglow.jsonschema



private[jsonschema] trait USumTypes { this: UContext with UCommons with UValueTypes with UProductTypes =>
  import c.universe._


  class SumTypeExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.OneOf] = {

      def isSealed(x: Type): Boolean = {
        val s = x.typeSymbol
        s.isClass && s.asClass.isSealed
      }

      def isSupportedLeafType(x: Type): Boolean = {
        val s = x.typeSymbol
        s.isClass && !s.isModuleClass && s.asClass.isCaseClass
      }

      // BORROWED:
      // https://github.com/plokhotnyuk/jsoniter-scala/blob/3612fddf19a8ce23ac973d71e85ef02f79c06fff/jsoniter-scala-macros/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L351-L365
      def collectRecursively(x: Type): Seq[Type] =
        if (x.typeSymbol.isClass) {
          val leafs = x.typeSymbol.asClass.knownDirectSubclasses.toSeq flatMap { s =>
            val cs = s.asClass
            val subTpe = if (cs.typeParams.isEmpty) cs.toType else resolveGenericType(cs.toType, cs.typeParams, x.typeArgs)
            if (isSealed(subTpe)) collectRecursively(subTpe)
            else if (isSupportedLeafType(subTpe)) Seq(subTpe)
            else c.abort(
              c.enclosingPosition,
              "Only Scala case classes are supported for Sum Type leafs. Please consider using of " +
              s"them for Sum Types with base '$x' or provide a custom implicitly accessible json.Schema for the Sum Type.")
          }
          if (isSupportedLeafType(x)) leafs :+ x else leafs
        } else Seq.empty

      Some.when (isSealed(tpe)) {
        val schemas = collectRecursively(tpe) map {
          case ValueClass(st) => st
          case CaseClass(st)  => st
          case _              => c.abort(c.enclosingPosition, "") // TODO: proper error message
        }
        U.OneOf(tpe, schemas)
      }
    }
  }

  val SumType = new SumTypeExtractor
}
