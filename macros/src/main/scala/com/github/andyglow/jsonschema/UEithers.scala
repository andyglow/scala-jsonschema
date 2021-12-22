package com.github.andyglow.jsonschema

import json.Profile

private[jsonschema] trait UEithers { this: UContext with UCommons =>
  import c.universe._
  import SchemaType._

  class EitherExtractor {

    def unapply(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] = {
      Some.when (tpe <:< T.either) {
        val asOneOf = ctx.has[Profile.EitherAsOneOf]
        val asLR    = ctx.has[Profile.EitherAsLeftOrRight]

        if (asOneOf && asLR) abort("Usage of both EitherAsOneOf and EitherAsLeftOrRight is ambiguous") else {
          val List(leftType, rightType) = tpe.typeArgs
          val leftSchema    = resolve(leftType, ctx :+ tpe)
          val rightSchema   = resolve(rightType, ctx :+ tpe)

          if (asOneOf) {
            SchemaType.OneOf(
              tpe,
              List(
                leftSchema,
                rightSchema),
              None)
          } else if (asLR) {
            val (l, r) = ctx.leftRightSymbols

            SchemaType.OneOf(
              tpe,
              List(
                Obj(
                  tpe,
                  Seq(
                    Obj.Field.Apply(leftType, l.name.decodedName.toString, leftSchema, Some(q"true"), None, None, None))),
                Obj(
                  tpe,
                  Seq(
                    Obj.Field.Apply(rightType, r.name.decodedName.toString, rightSchema, Some(q"true"), None, None, None)))),
              None)
          } else {
            abort("Standard scala.Either support is not enabled. Please provide json.Profile instance with Either Support enabled")
          }
        }
      }
    }
  }

  val Eithr = new EitherExtractor
}
