package com.github.andyglow.jsonschema

private[jsonschema] trait URecursiveTypes { this: UContext with SchemaTypes with USignatures =>
  import c.universe._

  class RecursiveTypes {

    private var types: List[Type] = Nil

    def append(x: Type): Unit = {
      if (types.exists(_ =:= x)) ()
      else {
        types = types :+ x
      }
    }

    /** Traverses through the schema tree and replaces top recursive types with Ref, so that it would go to definition when rendered
      *
      * @param in
      * @return
      */
    def substitute(in: SchemaType): SchemaType = {
      import SchemaType._

      transformSchema(in) {
        case st if types.exists(_ =:= st.tpe) =>
          st match {
            case _: Def | _: Ref => st
            case st              => Def(st.tpe, q"${signature(st.tpe)}", st)
          }
        case st => st
      }
    }
  }
}
