package com.github.andyglow.jsonschema

private[jsonschema] trait UDictionaries {
  this: UContext with UCommons with UEnums with UValueTypes =>
  import c.universe._

  class DictionaryExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Dict] = {
      Option.whenever(tpe <:< T.map) {
        val containerTpe = tpe.typeConstructor
        val keyTpe       = tpe.typeArgs.head
        val valueTpe     = tpe.typeArgs.tail.head
        val valueSchema  = resolve(valueTpe, ctx :+ tpe)
        val (keyIsString, keyVC) = keyTpe match {
          case T.string                                    => (true, None)
          case ValueClass(U.ValueClass(_, T.string, _, _)) => (true, Some(T.string))
          case ValueClass(U.ValueClass(_, innerTpe, _, _)) => (false, Some(innerTpe))
          case _                                           => (false, None)
        }

        val dict = U.Dict(keyTpe, valueTpe, containerTpe, valueSchema)

        val effectiveDict: Option[U.Dict] = if (!keyIsString) {
          // lookup for KeyPattern[_]
          c.inferImplicitValue(appliedType(T.keyPattern, keyVC getOrElse keyTpe)) match {
            // +---------
            // | no KeyPattern found
            // +------------------------
            case EmptyTree =>
              keyTpe match {
                // +---------
                // | key is enum
                // +------------------------
                case Enum(U.Enum(_, _: SchemaType.Str, values, _)) =>
                  val names   = values collect { case (_, Some(name)) => name }
                  val pattern = names.mkString("^(?:", "|", ")$")
                  dict.copy(extra = dict.extra.copy(validations = Seq(q"${N.Validation}.patternProperties := $pattern")))

                // return None otherwise
                case _ => None
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

        c.info(c.enclosingPosition, "1: " + show(dict), force = true)
        c.info(c.enclosingPosition, "2: " + show(effectiveDict), force = true)

        effectiveDict
      }
    }
  }

  val Dict = new DictionaryExtractor
}
