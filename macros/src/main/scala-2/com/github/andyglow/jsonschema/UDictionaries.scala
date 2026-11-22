package com.github.andyglow.jsonschema

private[jsonschema] trait UDictionaries {
  this: UContext with UCommons with UEnums with UValueTypes =>
  import c.universe._

  class DictionaryExtractor {

    private def unsupportedKey(tpe: Type, keyTpe: Type, why: String): Nothing =
      c.abort(
        c.enclosingPosition,
        s"schema for $tpe is not supported: a Map key must be a String, a value class of String, an enum or a type with an implicit " +
          s"json.Schema.dictionary.KeyPattern; the key type $keyTpe $why. Provide an implicit KeyPattern[$keyTpe], or a json.schema.Predef or json.Schema for the Map"
      )

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[U.Dict] = {
      if (tpe <:< T.anyMap && !(tpe <:< T.map))
        c.abort(
          c.enclosingPosition,
          s"schema for $tpe is not supported: only immutable Maps (scala.collection.immutable.Map) are derived as dictionaries. Provide a json.schema.Predef or json.Schema for it"
        )

      Option.whenever(tpe <:< T.map) {
        val containerTpe = tpe.typeConstructor
        val mapArgs      = tpe.baseType(T.map.typeSymbol).typeArgs
        // the generated `dictionary[K, V, C]` needs a container C[_, _] that takes the key and value types as its only type parameters
        // (not so for e.g. IntMap[V], or a `class UserMap extends Map[String, User]`)
        if (tpe.typeArgs.size != 2 || !tpe.typeArgs.zip(mapArgs).forall { case (a, b) => a =:= b })
          c.abort(
            c.enclosingPosition,
            s"schema for $tpe is not supported: a Map must take its key and value types as its only type parameters (like Map[K, V]), and $tpe is a Map[${mapArgs.mkString(", ")}]. " +
              s"Provide a json.schema.Predef or json.Schema for it"
          )
        val keyTpe               = tpe.typeArgs.head
        val valueTpe             = tpe.typeArgs.tail.head
        val valueSchema          = resolve(valueTpe, ctx :+ tpe)
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

                // a family of case objects (what 0.7 derived as an enum)
                case Enum(f: U.Family) if f.enumsAsOneOf => unsupportedKey(tpe, keyTpe, "is an enum, but with Flag.EnumsAsOneOf its members aren't plain strings")
                case Enum(f: U.Family) if f.members.forall(_.schema.isInstanceOf[SchemaType.Str]) =>
                  val names   = f.members.flatMap(_.knownString)
                  val pattern = names.mkString("^(?:", "|", ")$")
                  dict.copy(extra = dict.extra.copy(validations = Seq(q"${N.Validation}.patternProperties := $pattern")))
                case Enum(_) => unsupportedKey(tpe, keyTpe, "is an enum of non-string values")
                case _       => unsupportedKey(tpe, keyTpe, "is none of these")
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

        effectiveDict
      }
    }
  }

  val Dict = new DictionaryExtractor
}
