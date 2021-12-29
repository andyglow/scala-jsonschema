package com.github.andyglow.jsonschema

import scala.reflect.internal.util.NoSourceFile

private[jsonschema] trait UImplicits { this: UContext with UCommons with USignatures =>
  import c.universe._

  class Implicit {
    final val LF = '\u000A'
    final val CR = '\u000D'

    sealed trait ImplicitSchema
    case class FromPredef(x: Tree) extends ImplicitSchema
    case class FromSchema(x: Tree) extends ImplicitSchema
    case object NotFound           extends ImplicitSchema

    // FIXME: this method is pretty ugly
    //   it is trying to handle situation like this
    //   {{{
    //     case class CC(a: String)
    //     implicit val aSchema: Schema[CC] = Json.schema[CC]
    //   }}}
    //   in this case `inferImplicitValue` returns a self-reference
    //   which turns out to be resolved as `null` eventually
    //
    //   So this method take a line of a source code where our def-macro is used and
    //   see if it is assignment and it assigns to variable returned by `inferImplicitValue`
    //
    def isSelfRef(x: Tree): Boolean = x match {
      case x @ Select(This(_), TermName(field)) =>
        val pos = x.pos
        pos.source match {
          case NoSourceFile => false
          case src =>
            val end   = src.lineToOffset(src.offsetToLine(pos.point))
            var start = end - 2

            while ({
              !src.isLineBreak(start) &&
              !src.isEndOfLine(start)
            }) start = start - 1

            val str = new String(src.content, start, pos.point - start)
            if (str.contains(field) && str.contains("=") && str.contains("implicit")) true
            else false
        }
      case _ => false
    }

    def get(tpe: Type): Option[SchemaType] = {

      val sType = appliedType(T.schemaC, tpe) // .dealias.widen
      val pType = appliedType(T.predefC, tpe) // .dealias.widen

      // search implicit scope for a schema
      //
      // this 2 level implicit search is done this way because Schema/Predef
      // are covariants which makes impossible use of implicit conversions like
      // ```
      // def schemaFromPredef[T](implicit p: Predef[T]): Schema[T] = p.schema
      // ```
      // if taken from schema - must be exposed as `ref`
      // if taken from predef - exposing as is
      def lookupSchema: ImplicitSchema = {
        // schema
        c.inferImplicitValue(sType) match {
          case EmptyTree =>
            // predef
            c.inferImplicitValue(pType) match {
              case EmptyTree => NotFound
              case x         => FromPredef(q"$x.schema")
            }
          case x if isSelfRef(x) => NotFound
          case x                 => FromSchema(x)
        }
      }

      lookupSchema match {
        case NotFound      => None
        case FromPredef(x) => Some(U.`-from-tree-`(tpe, x))
        case FromSchema(x) =>
          Some(U.`-from-tree-`(tpe, q"""${N.Schema}.`def`[$tpe]($x)(${signature(tpe)})"""))
      }
    }

    def getOrElse(tpe: Type, gen: => SchemaType): SchemaType = get(tpe) getOrElse gen
  }
  val Implicit = new Implicit
}
