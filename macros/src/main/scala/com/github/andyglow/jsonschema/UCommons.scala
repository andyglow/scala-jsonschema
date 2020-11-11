package com.github.andyglow.jsonschema

import com.github.andyglow.json.ToValue
import json.Schema.`dictionary`.KeyPattern

import scala.language.implicitConversions
import scala.util.control.NonFatal


private[jsonschema] trait UCommons extends SchemaTypes with ULogging { this: UContext with UFieldDecorations =>
  import c.universe._

  class ResolutionContext(val stack: List[Type], val onCycle: Type => Unit) {
    def isEmpty: Boolean = stack.isEmpty
    def contains(x: Type): Boolean = stack exists { y => x =:= y }
    def :+(x: Type): ResolutionContext = new ResolutionContext(stack :+ x, onCycle)
  }

  def resolve(
    tpe: Type,
    ctx: ResolutionContext,
    specFD: FieldDecorations = FieldDecorations.Empty): SchemaType

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
  private[jsonschema] object N {
    val json       = q"_root_.json"
    val Json       = q"$json.Json"
    val Schema     = q"$json.Schema"
    val Predef     = q"$json.schema.Predef"
    val Validation = q"$json.Validation"
    final object internal {
      private val prefix = q"_root_.com.github.andyglow"
      val json = q"$prefix.json"
      val jsonschema = q"$prefix.jsonschema"
      val TypeSignature = q"$jsonschema.TypeSignature"
    }
  }

  // T is for Types
  private[jsonschema] object T {
    val string        = typeOf[String]
    val array         = typeOf[Array[_]]
    val iterable      = typeOf[Iterable[_]]
    val option        = weakTypeOf[Option[_]]
    val toValue       = weakTypeOf[ToValue[_]]
    val set           = weakTypeOf[Set[_]]
    val map           = weakTypeOf[Map[_, _]]
    val lazyRef       = typeOf[json.Schema.`lazy-ref`[_]]
    val keyPattern    = typeOf[KeyPattern[_]]
    val schemaC       = typeOf[json.Schema[_]].typeConstructor
    val predefC       = typeOf[json.schema.Predef[_]].typeConstructor

    object decoration {
      val title = typeOf[json.schema.title]
      val description = typeOf[json.schema.description]
    }
  }

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
        if (clazz.isDerivedValueClass) {
          None
        } else {
          Some(block)
        }
      } else
        None
    } else
      None
  }

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
  }
}
