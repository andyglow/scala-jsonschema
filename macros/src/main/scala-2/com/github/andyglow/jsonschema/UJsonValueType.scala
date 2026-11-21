package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value

private[jsonschema] trait UJsonValueType { this: UTypeAnnotations with UContext with UCommons =>

  import c.universe._

  /** Extracts Schemas from json creation trees e.g.
    * {{{
    *   Value.str("abc") -> SchemaType.Str    (typeOf[String], q"None")
    *   Value.num(5)     -> SchemaType.Integer()
    *   Value.num(3.14)  -> SchemaType.Number (typeOf[Double])
    * }}}
    */
  class JsonValueTypeExtractor {

//    def fromTrees(xs: Seq[Tree]): SchemaType = if (xs.isEmpty) abort("empty trees") else {
//    }

    def unapply(x: Tree): Option[SchemaType] = {
      val xx = c.untypecheck(x)
//      dbg("LEAF1: " + showRaw(x))

      xx match {
        case Apply(
              Select(
                Select(
                  Select(
                    Select(
                      Select(Select(Ident(termNames.ROOTPKG), TermName("com")), TermName("github")),
                      TermName("andyglow")
                    ),
                    TermName("json")
                  ),
                  TermName("Value")
                ),
                TermName("num")
              ),
              List(v)
            ) =>
          v match {
            case Literal(Constant(_: Int))    => SchemaType.Integer()
            case Literal(Constant(_: Byte))   => SchemaType.Integer()
            case Literal(Constant(_: Short))  => SchemaType.Integer()
            case Literal(Constant(_: Double)) => SchemaType.Number(typeOf[Double])
            case Literal(Constant(_: Float))  => SchemaType.Number(typeOf[Float])
            case Literal(Constant(_: Long))   => SchemaType.Number(typeOf[Long])
            case Literal(Constant(_: BigInt)) => SchemaType.Number(typeOf[BigInt])
          }
        case _ =>
          val tx  = c.typecheck(x)
          val tpe = tx.tpe
//          dbg("LEAF2: " + showRaw(tx))
//          dbg("LEAF3: " + showRaw(tpe))
//          dbg("LEAF4: " + showCode(tx))

          tpe match {
            case t if t =:= typeOf[Value.str]  => SchemaType.Str(typeOf[String], q"None")
            case t if t =:= typeOf[Value.bool] => SchemaType.Bool()
            case t if t =:= typeOf[Value.num]  => SchemaType.Number(typeOf[Long])
            case t if t =:= typeOf[Value]      =>
//              val v = try c.eval(c.Expr.apply[Value](c.prefix.mirror, TreeCreator(tx))) catch { case ex: Throwable =>
//                val sre = ScalaReflectionException(s"Error evaluating: ```\n${showCode(tx, printTypes = true, printRootPkg = true, printIds = true, printOwners = true)}\n````")
//                sre.initCause(ex)
//                throw sre
//              }
//              v match {
//                case _: Value.str  => SchemaType.Str(typeOf[String], q"None")
//                case _: Value.bool => SchemaType.Bool()
//                case _: Value.num  => SchemaType.Number(typeOf[Long])
//                case _ => None
//              }
              None
            case _ => abort(s"illegal json value type: ${show(tpe)}")
          }
      }
    }
  }

  val JsonValueType = new JsonValueTypeExtractor
}
