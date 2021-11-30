package com.github.andyglow.jsonschema

import com.github.andyglow.json.ToValue
import json.Schema.`dictionary`.KeyPattern

import scala.language.implicitConversions
import scala.util.control.NonFatal


private[jsonschema] trait UCommons extends SchemaTypes with ULogging { this: UContext with UFieldDecorations =>
  import c.universe._

  lazy val is211 = util.Properties.versionNumberString.startsWith("2.11")

  class ResolutionContext(val stack: List[Type], val onCycle: Type => Unit) {
    def isEmpty: Boolean = stack.isEmpty
    def contains(x: Type): Boolean = stack exists { y => x =:= y }
    def :+(x: Type): ResolutionContext = new ResolutionContext(stack :+ x, onCycle)
  }

  def resolve(
    tpe: Type,
    ctx: ResolutionContext,
    specFD: FieldDecorations = FieldDecorations.Empty,
    noImplicit: Boolean = false
  ): SchemaType

  def resolveGenericType(x: Type, from: List[Symbol], to: List[Type]): Type = {
    try x.substituteTypes(from, to) catch {
      case NonFatal(_) =>
        c.abort(
          c.enclosingPosition,
          s"""Cannot resolve generic type(s) for `$x`
             |Please provide a custom implicitly accessible json.Schema for it.
             |""".stripMargin)
    }
  }

  // U is for Unit
  val U = SchemaType

  // N is for Names
  private[jsonschema] class ConstantNames {
    val json       = q"_root_.json"
    val Json       = q"$json.Json"
    val Schema     = q"$json.Schema"
    val Field      = q"$Schema.`object`.Field"
    val Predef     = q"$json.schema.Predef"
    val Validation = q"$json.schema.validation.Instance"
    object internal {
      private val prefix = q"_root_.com.github.andyglow"
      val json = q"$prefix.json"
      val jsonschema = q"$prefix.jsonschema"
      val TypeSignature = q"$jsonschema.TypeSignature"
    }
  }
  private[jsonschema] val N = new ConstantNames

  // T is for Types
  private[jsonschema] class ConstantTypes {
    val string        = typeOf[String]
    val array         = typeOf[Array[_]]
    val iterable      = typeOf[Iterable[_]]
    val option        = weakTypeOf[Option[_]]
    val toValue       = weakTypeOf[ToValue[_]]
    val set           = weakTypeOf[Set[_]]
    val map           = weakTypeOf[Map[_, _]]
    val lazyRef       = typeOf[json.Schema.`ref`[_]]
    val keyPattern    = typeOf[KeyPattern[_]]
    val schemaC       = typeOf[json.Schema[_]].typeConstructor
    val predefC       = typeOf[json.schema.Predef[_]].typeConstructor

    object annotation {
      val title             = typeOf[json.schema.title]
      val description       = typeOf[json.schema.description]
      val definition        = typeOf[json.schema.definition]
      val discriminator     = typeOf[json.schema.discriminator]
      val discriminatorKey  = typeOf[json.schema.discriminatorKey]
      val typeHint          = typeOf[json.schema.typeHint[_]]
      val readOnly          = typeOf[json.schema.readOnly]
      val writeOnly         = typeOf[json.schema.writeOnly]
    }

    def isNothing(t: Type): Boolean = t.dealias match {
      case t: TypeRef if t.sym == definitions.NothingClass => true
      case _                                               => false
    }
  }
  private[jsonschema] val T = new ConstantTypes

  def validateNonValueCaseClass[T](tpe: Type, prefix: String)(block: => T): T = {
    val symbol = tpe.typeSymbol
    if (symbol.isClass) {
      val clazz = symbol.asClass
      if (clazz.isCaseClass) {
        if (clazz.isDerivedValueClass) {
          c.abort(c.enclosingPosition, s"$prefix: can't be used against value classes")
        } else {
          block
        }
      } else
        c.abort(c.enclosingPosition, s"$prefix: can't be used against non-case classes")
    } else
      c.abort(c.enclosingPosition, s"$prefix: can be used only against case classes")
  }

  def forNonValueCaseClass[T](tpe: Type)(block: => T): Option[T] = {
    val symbol = tpe.typeSymbol
    if (symbol.isClass) {
      val clazz = symbol.asClass
      if (clazz.isCaseClass) {
        if (clazz.isDerivedValueClass || clazz.isModuleClass) {
          None
        } else {
          Some(block)
        }
      } else
        None
    } else
      None
  }

  def isSealed(x: Type): Boolean = {
    val s = x.typeSymbol
    s.isClass && s.asClass.isSealed
  }

  def isCaseClass(x: Type): Boolean = {
    val s = x.typeSymbol
    s.isClass && !s.isModuleClass && s.asClass.isCaseClass
  }

  def isCaseObject(x: Type): Boolean = isCaseObject(x.typeSymbol)

  def isCaseObject(x: SymbolApi): Boolean = x.isClass && x.isModuleClass

  // BORROWED:
  // https://github.com/plokhotnyuk/jsoniter-scala/blob/3612fddf19a8ce23ac973d71e85ef02f79c06fff/jsoniter-scala-macros/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L351-L365
  def resolveSumTypeRecursively(
    tpe: Type,
    include: Type => Boolean,
    otherwise: Symbol => Type): Seq[Type] = {

    if (tpe.typeSymbol.isClass) {
      val leaves = tpe.typeSymbol.asClass.knownDirectSubclasses.toSeq flatMap { s =>
        val cs = s.asClass
        val subTpe = if (cs.typeParams.isEmpty) cs.toType else resolveGenericType(cs.toType, cs.typeParams, tpe.typeArgs)
        if (isSealed(subTpe)) resolveSumTypeRecursively(subTpe, include, otherwise)
        else if (include(subTpe)) Seq(subTpe)
        else Seq(otherwise(s))
      }
      if (include(tpe)) leaves :+ tpe else leaves
    } else Seq.empty
  }

  class TypeOps(x: Type) {

    def opt: Option[Type] = if (x == NoType) None else Some(x)
  }

  implicit def mkTypeOps(x: Type): TypeOps = new TypeOps(x)

  implicit class SomeCompanionOps(val x: Some.type) {
    def when[T](predicate: Boolean)(block: => T): Option[T] = if (predicate) Some(block) else None
  }

  implicit class OptionCompanionOps(val x: Option.type) {
    def whenever[T](predicate: Boolean)(block: => Option[T]): Option[T] = if (predicate) block else None
  }

  implicit def asSome[T](x: T): Option[T] = Some(x)

  case class Field(
    name: TermName,
    tpe: Type,
    effectiveTpe: Type,
    annotations: List[Annotation],
    default: Option[Tree],
    isOption: Boolean) {

    def hasDefault: Boolean = default.isDefined

    object hasAnnotation {

      lazy val readOnly: Boolean = annotations.map(_.tree.tpe).exists(_ <:< T.annotation.readOnly)

      lazy val writeOnly: Boolean = annotations.map(_.tree.tpe).exists(_ <:< T.annotation.writeOnly)
    }
  }
}
