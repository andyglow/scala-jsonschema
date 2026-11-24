package com.github.andyglow.jsonschema

import scaladoc._

trait UFieldDecorations { this: UContext with UCommons =>
  import c.universe._

  /** A map of field name to description Can be inferred from
    *   - annotations [[FieldDecorations.fromFieldAnnotations]]
    *   - scaladoc [[FieldDecorations.fromScaladoc]]
    *   - or given explicitly [[FieldDecorations.fromSpec]
    */
  type FieldDecorations = Map[String, String]

  object FieldDecorations {

    val Empty: FieldDecorations = Map.empty

    /** Used in conjunction with Macroses.deriveObjectSchema
      * {{{
      *   val schema = Json.objectSchema[MyCaseClass](
      *     "fieldName1" -> "A description of the First field",
      *     "fieldName2" -> "A description of the Second field")
      * }}}
      *
      * @param exprs
      * @return
      */
    def fromSpec(exprs: Seq[c.Expr[(String, String)]]): FieldDecorations = {

      def unsupported(): Nothing =
        c.abort(
          c.enclosingPosition,
          """only constant Strings are allowed in Description specification. Example: "id" -> "Record ID", ... """ +
            """(each pair written out as "key" -> "value" or ("key", "value"), not a Seq passed as varargs)"""
        )

      def fromTree(x: Tree): String = x match {
        case Literal(Constant(v: String)) => v
        case _                            => unsupported()
      }

      val pairs = exprs.map { d =>
        d.tree match {
          // case "key" -> "val" // scala 2.12
          case Apply(
                TypeApply(
                  Select(
                    Apply(
                      TypeApply(
                        Select(
                          Select(Ident(TermName("scala")), TermName("Predef")),
                          TermName("ArrowAssoc")
                        ),
                        List(TypeTree())
                      ),
                      List(k)
                    ),
                    TermName("$minus$greater")
                  ),
                  List(TypeTree())
                ),
                List(v)
              ) =>
            (fromTree(k), fromTree(v))
          // case "key" -> "val" // scala 2.11
          case Apply(
                TypeApply(
                  Select(
                    Apply(
                      TypeApply(
                        Select(
                          Select(This(TypeName("scala")), TermName("Predef")),
                          TermName("ArrowAssoc")
                        ),
                        List(TypeTree())
                      ),
                      List(k)
                    ),
                    TermName("$minus$greater")
                  ),
                  List(TypeTree())
                ),
                List(v)
              ) =>
            (fromTree(k), fromTree(v))
          // case ("key", "val")
          case Apply(
                TypeApply(
                  Select(Select(Ident(TermName("scala")), TermName("Tuple2")), TermName("apply")),
                  List(TypeTree(), TypeTree())
                ),
                List(k, v)
              ) =>
            (fromTree(k), fromTree(v))
          case _ => unsupported()
        }
      }

      pairs.toMap
    }

    def fromScaladoc(tpe: Type, scaladoc: Option[Scaladoc]): FieldDecorations =
      scaladoc.fold(Empty) { x =>
        x.textParams
      }

    def fromFieldAnnotations(fields: Seq[Field]): FieldDecorations = {
      fields.flatMap { f =>
        val descr = f.annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.description)
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

        descr map { (f.name.decodedName.toString, _) }
      }.toMap
    }

    /** Field titles by field name, from `@title` on the parameter or its field (`@(title @field)`). They go on the field's schema: `object.Field` has no title. */
    def titlesFromFieldAnnotations(fields: Seq[Field]): Map[String, String] = {
      fields.flatMap { f =>
        val title = f.annotations
          .map(_.tree)
          .filter(_.tpe <:< T.annotation.title)
          .collectFirst { case Apply(_, List(Literal(Constant(text: String)))) => text }

        title map { (f.name.decodedName.toString, _) }
      }.toMap
    }

    implicit class FieldDescriptionsOps(val fd: FieldDecorations) {

      def warnUnknown(tpe: Type, fields: Seq[Field]): Unit = {
        val names   = fields.map(_.name.decodedName.toString)
        val unknown = fd.keys.filterNot(names.contains).toList.sorted
        if (unknown.nonEmpty) {
          val sym = tpe.typeSymbol
          val pos = if (sym.pos != NoPosition) sym.pos else c.enclosingPosition
          c.warning(pos, s"scaladoc of ${show(tpe)} has @param for unknown field(s): ${unknown.mkString(", ")}. ${show(tpe)} fields are: ${names.mkString(", ")}")
        }
      }

      def validate(tpe: Type, fields: Seq[Field]): Unit = {
        fd.keySet foreach { k =>
          if (!fields.exists(_.name.decodedName.toString == k))
            c.abort(
              c.enclosingPosition,
              s"unknown field: $k. ${show(tpe)} fields are: ${fields.map(_.name.decodedName.toString).mkString(", ")}"
            )
        }
      }
    }
  }
}
