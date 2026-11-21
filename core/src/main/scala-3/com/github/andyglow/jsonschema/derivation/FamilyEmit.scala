package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.json.Value
import json.Schema

/** Emission of `Schema.family` (0.9): a sealed family ([[Sums.Sum]] of kind `Sealed`, and the [[Enums.EnumFamily]] of a sealed family or Scala 3 `enum` of singletons) is emitted as its facts, the
  * same tree the Scala 2 macro emits. A [[json.schema.JsonSchemaProfile]] shapes it when the schema is rendered: Neutral and Legacy to what [[Sums.emitSum]] / [[Enums.EnumFamily]] emitted before
  * (still used for `Either`).
  */
trait FamilyEmit { this: Derivation =>
  import q.reflect.*

  /** One member's facts, the expressions already built. */
  private final case class MemberFacts(
    name: String,
    fullName: String,
    sig: String,
    kind: Expr[Schema.`family`.Kind],
    schema: Expr[Schema[?]],
    value: Option[Expr[Value]],
    cls: Option[TypeRepr],
    texts: Extra,
    discriminatorKey: Option[String],
    definition: Option[Arg[String]],
    valueField: Option[String]
  )

  /** A symbol's full name as Scala 2's `fullName` prints it: no `$` of objects (`petshop.Animal.Dog`, `petshop.Fish`). */
  private def scala2FullName(sym: Symbol): String = sym.fullName.split('.').map(_.stripSuffix("$")).mkString(".")

  def emitSumFamily(s: Sum): Expr[Schema[?]] = {
    val members = s.leaves map { m =>
      val sym = if (m.singleton.isDefined) m.singleton.get.symbol else classSymbol(m.tpe)
      // a Scala 3 enum case's runtime class doesn't identify it (reviewer11 A6)
      val cls                                                                                                                      = Option.when(m.kind != MemberKind.EnumSingletonCase)(m.tpe.widen)
      def facts(kind: Expr[Schema.`family`.Kind], schema: Expr[Schema[?]], value: Option[Expr[Value]], valueField: Option[String]) =
        MemberFacts(m.name, scala2FullName(sym), m.sig, kind, schema, value, cls, m.texts, m.discriminatorKey, m.definition, valueField)
      m.schema match {
        case sg: Singleton   => facts('{ Schema.`family`.Kind.Singleton }, emit(sg.valueSchema), Some(sg.item.value), None)
        case n: FromImplicit => facts('{ Schema.`family`.Kind.Provided }, emit(n), None, None)
        case vc: ValueClass  => facts('{ Schema.`family`.Kind.ValueClass }, emit(vc), None, Some(vc.fieldName))
        case n               => facts('{ Schema.`family`.Kind.Product }, emit(n), None, None)
      }
    }
    emitFamily(s.tpe, members, s.discriminator.map(resolved), s.enumsAsOneOf)
  }

  /** `@discriminator`'s field and phantom, defaults resolved (a non-literal was rejected during construction). */
  private def resolved(d: TypeAnnotations.Discriminator): (String, Boolean) = {
    val field = d.field match {
      case Arg.Given(f) => f
      case _            => "type"
    }
    val phantom = d.phantom match {
      case Arg.Given(b) => b
      case _            => true
    }
    (field, phantom)
  }

  def emitEnumFamily(e: EnumFamily): Expr[Schema[?]] = {
    val vs      = emit(e.valueSchema)
    val members = e.items map { i =>
      val sym        = i.ref.symbol
      val cls        = Option.when(i.kind != ValueKind.EnumSingletonCase)(i.tpe.widen)
      val definition = symbolAnnotations(List(sym, sym.moduleClass).filterNot(_.isNoSymbol).distinct).definition
      MemberFacts(i.name, scala2FullName(sym), i.sig, '{ Schema.`family`.Kind.Singleton }, vs, Some(i.value), cls, i.texts, i.discriminatorKey, definition, None)
    }
    // recorded as Scala 2 does; the lowering ignores @discriminator on a family of singletons, as 0.7 did
    emitFamily(e.tpe, members, typeAnnotations(e.tpe).discriminator.map(resolved), e.enumsAsOneOf)
  }

  private def emitFamily(tpe: TypeRepr, members: List[MemberFacts], discriminator: Option[(String, Boolean)], enumsAsOneOf: Boolean): Expr[Schema[?]] =
    tpe.asType match {
      case '[t] =>
        val ms = Expr.ofList(members.map(emitMember))
        val d  = discriminator match {
          case Some((f, p)) =>
            val fe = Expr(f)
            val pe = Expr(p)
            '{ Some(Schema.`family`.Discriminator($fe, $pe)) }
          case None => '{ None }
        }
        val eo = Expr(enumsAsOneOf)
        '{ Schema.`family`[t]($ms, $d, $eo) }
    }

  private def emitMember(m: MemberFacts): Expr[Schema.`family`.Member] = {
    def str(x: Option[String]): Expr[Option[String]] = Expr(x)
    val name                                         = Expr(m.name)
    val fullName                                     = Expr(m.fullName)
    val sig                                          = Expr(m.sig)
    val kind                                         = m.kind
    val schema                                       = m.schema
    val value: Expr[Option[Value]]                   = m.value match {
      case Some(v) => '{ Some($v) }
      case None    => '{ None }
    }
    val cls: Expr[Option[Class[?]]] = m.cls match {
      case Some(t) =>
        val c = Literal(ClassOfConstant(t)).asExprOf[Class[?]]
        '{ Some($c) }
      case None => '{ None }
    }
    val title                                                = str(m.texts.title)
    val description                                          = str(m.texts.description)
    val key                                                  = str(m.discriminatorKey)
    val definition: Expr[Option[Schema.`family`.Definition]] = m.definition match {
      case Some(Arg.Given(n)) =>
        val ne = Expr(n)
        '{ Some(Schema.`family`.Definition.Named($ne)) }
      case Some(Arg.Default) => '{ Some(Schema.`family`.Definition.Inferred) }
      case _                 => '{ None }
    }
    val valueField = str(m.valueField)
    '{ Schema.`family`.Member($name, $fullName, $sig, $kind, $schema, $value, $cls, $title, $description, $key, $definition, $valueField) }
  }
}
