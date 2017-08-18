package com.github.andyglow.jsonschema

import java.util.UUID

import json.Schema

import scala.language.postfixOps
import scala.reflect.macros.blackbox

object SchemaMacro {

  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Schema] = {
    import c.universe._

    val typeMap: Tree = {
      val typeMap = c.inferImplicitValue(typeOf[TypeRegistry])
      if (typeMap.isEmpty) q"TypeMap.empty" else typeMap
    }

    case class FieldInfo(name: TermName, tpe: Type, annotations: List[Annotation], hasDefault: Boolean)

    def fieldMap(tpe: Type): Seq[FieldInfo] = {

      val annotationMap = tpe.decls.collect {

        case s: MethodSymbol if s.isCaseAccessor =>
          // workaround: force loading annotations
          s.typeSignature
          s.accessed.annotations.foreach(_.tree.tpe)

          s.name.toString.trim -> s.accessed.annotations
      }.toMap

      object Field {

        def unapply(s: TermSymbol): Option[FieldInfo] = {
          val name = s.name.toString.trim
          if ( s.isVal
            && s.isCaseAccessor) {
            Some(FieldInfo(
              name        = TermName(name),
              tpe         = s.infoIn(tpe),
              annotations = annotationMap.getOrElse(name, List.empty),
              hasDefault  = s.isParamWithDefault))
          } else None
        }
      }

      tpe.decls.collect {case Field(f) => f} toSeq
    }

    val optionTpe = weakTypeOf[Option[_]]

    def resolve(tpe: Type, stack: List[Type]): Tree = {
      if (stack contains tpe) c.error(c.enclosingPosition, s"cyclic dependency for $tpe")

      def tree = tpe match {
        // boolean
        case x if x =:= typeOf[Boolean]                 => q"Schema(`boolean`)"

        // numeric
        case x if x =:= typeOf[Short]                   => q"""Schema(`integer`)"""
        case x if x =:= typeOf[Int]                     => q"""Schema(`integer`)"""
        case x if x =:= typeOf[Double]                  => q"""Schema(`number`)"""
        case x if x =:= typeOf[Float]                   => q"""Schema(`number`)"""
        case x if x =:= typeOf[Long]                    => q"""Schema(`number`)"""
        case x if x =:= typeOf[BigInt]                  => q"""Schema(`number`)"""
        case x if x =:= typeOf[BigDecimal]              => q"""Schema(`number`)"""

        // string
        case x if x =:= typeOf[String]                  => q"""Schema(`string`(None, None))"""

        // uuid
        case x if x =:= typeOf[UUID]                    => q"""Schema(`string`(None, Some("^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$$")))"""

        // date, date-time
        case x if x =:= typeOf[java.util.Date]          => q"""Schema(`string`(Some(`date`), None))"""
        case x if x =:= typeOf[java.sql.Date]           => q"""Schema(`string`(Some(`date`), None))"""
        case x if x =:= typeOf[java.sql.Timestamp]      => q"""Schema(`string`(Some(`date-time`), None))"""
        case x if x =:= typeOf[java.time.Instant]       => q"""Schema(`string`(Some(`date`), None))"""
        case x if x =:= typeOf[java.time.LocalDate]     => q"""Schema(`string`(Some(`date`), None))"""
        case x if x =:= typeOf[java.time.LocalDateTime] => q"""Schema(`string`(Some(`date-time`), None))"""
        case x if x =:= typeOf[java.time.LocalTime]     => q"""Schema(`string`(Some(`time`), None))"""

        case x if x <:< typeOf[Traversable[_]] =>
          val componentType = x.typeArgs.head
          val componentSchema = resolve(componentType, tpe +: stack)

          q"""Schema(`array`($componentSchema))"""

        case x =>
          val symbol = x.typeSymbol

          if (symbol.isClass) {
            val clazz = symbol.asClass
            if (clazz.isCaseClass) {
              if (!clazz.isDerivedValueClass) {
                val fields = fieldMap(x) map { f =>
                  val name          = f.name.decodedName.toString
                  val isOption      = f.tpe <:< optionTpe
                  val hasDefault    = f.hasDefault
                  val effectiveTpe  = if (isOption) f.tpe.typeArgs.head else f.tpe
                  val schema        = resolve(effectiveTpe, if (isOption) stack else tpe +: stack)

                  q"`object`.Field(name = $name, schema = $schema, required = ${ !isOption && !hasDefault })"
                }

                q"Schema(`object`(..$fields))"
              } else {
                val innerArg = clazz.primaryConstructor.asMethod.paramLists.head.head
                val innerType = innerArg.typeSignature

                resolve(innerType, tpe +: stack)
              }
            } else {
              c.error(c.enclosingPosition, s"schema for $clazz is not supported")
              q"""null"""
            }
          } else {
            c.error(c.enclosingPosition, s"schema for $x is not supported, $stack")
            q"""null"""
          }
      }

      val name = TypeSignatureMacro.typeSig(c)(tpe)

      q"""$typeMap.resolve($name, $tree)"""
    }

    val out = resolve(weakTypeOf[T], Nil)

    c.Expr[Schema] {
      q"""
        import json._
        import json.Type._

         $out
       """
    }
  }
}
