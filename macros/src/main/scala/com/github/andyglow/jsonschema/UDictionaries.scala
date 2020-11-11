package com.github.andyglow.jsonschema


private[jsonschema] trait UDictionaries { this: UContext with UCommons with UEnums =>
  import c.universe._

  class DictionaryExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Dict] = {
      Option.when (tpe <:< T.map) {
        val containerTpe  = tpe.typeConstructor
        val keyTpe        = tpe.typeArgs.head
        val valueTpe      = tpe.typeArgs.tail.head
        val valueSchema   = resolve(valueTpe, ctx :+ tpe)
        val keyIsString   = keyTpe =:= T.string

        val dict = U.Dict(keyTpe, valueTpe, containerTpe, valueSchema)

        if (!keyIsString) {
          // lookup for KeyPattern[_]
          c.inferImplicitValue(appliedType(T.keyPattern, keyTpe)) match {
            // +---------
            // | no KeyPattern found
            // +------------------------
            case EmptyTree =>
              keyTpe match {
                // +---------
                // | key is enum
                // +------------------------
                case Enum(U.Enum(_, _, Some(names), _)) =>
                  val pattern = names.mkString("^(?:", "|", ")$")
                  dict.copy(extra = dict.extra.copy(validations = Seq(q"${N.Validation}.patternProperties := $pattern")))
                case _                               => None
              }
            // +---------
            // | there is a KeyPattern type-class in the scope that brings in actual pattern
            // +------------------------
            case keyPattern =>
              dict.copy(extra = dict.extra.copy(validations = Seq(q"${N.Validation}.patternProperties := $keyPattern.pattern")))
          }
        } else {
          dict
        }
      }
    }
  }

  val Dict = new DictionaryExtractor
}
