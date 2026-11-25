package com.github.andyglow.jsonschema.derivation

import scala.collection.mutable.ListBuffer
import scala.quoted.*

import com.github.andyglow.json.Value
import json.Schema

/** Sum types: sealed traits / abstract classes (and Scala 3 `enum`s) with at least one member that isn't a singleton, and `Either` (Scala 2's `USumTypes`) -> `oneOf`.
  *
  * Construction builds a [[Sum]] whose members are in declaration order (`Symbol.children`), NOT flattened and NOT merged (profiledesign facts 2-4). A nested sealed family is a member of kind
  * [[MemberKind.NestedFamily]] whose schema is its own [[Sum]], built with the root's settings (`ToValue[Root]`, the root's `@typeHint`), as Scala 2 does. Every member keeps its simple name, its
  * signature, its own texts, `@discriminatorKey` and `@definition`; a singleton member is a [[Singleton]] node that keeps its value. Per member, an implicit `Schema`/`Predef` wins over derivation.
  * Generic members get the parent's type arguments by name (TRIAGE-B), via how the member extends the parent.
  *
  * Emit ([[Sum.emitRaw]]) shapes Scala 2's output from those facts:
  *   - nested families are flattened into one `oneOf`;
  *   - a case class / value class member gets its title, description and `@discriminatorKey` (an implicit-provided member gets none: Scala 2 quirk); with `@discriminator` on the root, the key
  *     defaults to the member's signature; then `@definition` wraps it in a definition (the key stays on the inner object);
  *   - singleton members become `enum`s, those with equal value schemas merged into one, their texts dropped; with `Flag.EnumsAsOneOf`, a `const` each, with its texts and key.
  *
  * `@discriminator` (phantom or not) is checked during construction: a non-phantom field must exist in every case class member (Scala 2's compile errors).
  */
trait Sums { this: Derivation =>
  import q.reflect.*

  enum SumKind {
    case Sealed, Either
  }

  enum MemberKind {
    case CaseClass, CaseObject, ValueClass, EnumClassCase, EnumSingletonCase, NestedFamily
  }

  /** @param name
    *   simple name (`Dog`)
    * @param sig
    *   full signature (`pkg.Animal.Dog`)
    * @param tpe
    *   the member's type (type arguments bound); a singleton's `TermRef`
    * @param singleton
    *   the value of a singleton member (case object, enum singleton case), for ToValue / profiles
    * @param texts
    *   the member's own `@title` / `@description` / scaladoc, ALWAYS recorded (Scala 2 drops them for case objects unless EnumsAsOneOf: done in emit)
    * @param definition
    *   `@definition` / `@definition("name")`
    * @param schema
    *   the member's own node, without texts / key / definition: an [[Nodes.Node]] of the product, value class or implicit, a [[Singleton]], or the nested [[Sum]]
    */
  final case class SumMember(
    name: String,
    sig: String,
    kind: MemberKind,
    tpe: TypeRepr,
    singleton: Option[Term],
    texts: Extra,
    discriminatorKey: Option[String],
    definition: Option[Arg[String]],
    schema: Node
  )

  /** @param discriminator
    *   `@discriminator` on the family, user literal vs annotation default kept ([[Arg]])
    * @param enumsAsOneOf
    *   the `Flag.EnumsAsOneOf` in scope, recorded; consulted in emit
    */
  final case class Sum(
    tpe: TypeRepr,
    kind: SumKind,
    members: List[SumMember],
    discriminator: Option[TypeAnnotations.Discriminator],
    typeHint: Option[TypeRepr],
    enumsAsOneOf: Boolean,
    extra: Extra = Extra()
  ) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = members.map(_.schema)
    def mapChildren(f: Node => Node): Node = copy(members = members.map(m => m.copy(schema = f(m.schema))))
    def emitRaw: Expr[Schema[?]]           = if (kind == SumKind.Either) emitSum(this) else emitSumFamily(this)

    /** The members with nested families flattened, in declaration order (depth first). */
    def leaves: List[SumMember] = members flatMap { m =>
      nestedSum(m.schema) match {
        case Some(n) if m.kind == MemberKind.NestedFamily => n.leaves
        case _                                            => List(m)
      }
    }
  }

  /** A singleton member of a sum: one enum value of the root family, with its value schema (Scala 2's `Extraction.Enumerated`). */
  final case class Singleton(tpe: TypeRepr, root: TypeRepr, item: EnumItem, valueSchema: Node, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(valueSchema)
    def mapChildren(f: Node => Node): Node = copy(valueSchema = f(valueSchema))
    def emitRaw: Expr[Schema[?]]           = emitEnum(root, valueSchema, List(item.value))
  }

  private def nestedSum(n: Node): Option[Sum] = n match {
    case s: Sum               => Some(s)
    case Def(_, _, s: Sum, _) => Some(s) // a nested family that is also recursive
    case _                    => None
  }

  // ---- construction

  def deriveSum(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    Option.when(isSealed(tpe)) {
      val ta      = typeAnnotations(tpe)
      val root    = RootSettings(tpe, toValueOf(tpe), ta.typeHint)
      val members = sealedChildren(tpe).map(member(_, root, ctx.descend(tpe)))
      val kind    = if (classSymbol(tpe) == TypeRepr.of[Either[Any, Any]].typeSymbol) SumKind.Either else SumKind.Sealed
      val sum     = Sum(tpe, kind, members, ta.discriminator, ta.typeHint, hasFlag[json.schema.Flag.EnumsAsOneOf])
      ta.discriminator.foreach(validateDiscriminator(_, sum.leaves))
      sum
    }

  /** What the members of a family (also of its nested families) take from the root. */
  private final case class RootSettings(tpe: TypeRepr, toValue: Option[Term => Expr[Value]], typeHint: Option[TypeRepr])

  private def member(c: SealedChild, root: RootSettings, ctx: Ctx): SumMember = {
    val name = c.sym.name
    val sig  = signature(c.tpe, dealiased = true)
    if (c.singleton) {
      if (!isCaseSingleton(c.sym)) unsupportedLeaf(c.sym, root.tpe)
      val item   = singletonItem(c, root.tpe, root.toValue)
      val ta     = symbolAnnotations(List(c.sym, c.sym.moduleClass).filterNot(_.isNoSymbol).distinct)
      val schema = lookupImplicit(c.tpe) getOrElse Singleton(c.tpe, root.tpe, item, valueSchemaOf(item, root.typeHint, ctx))
      val kind   = if (item.kind == ValueKind.EnumSingletonCase) MemberKind.EnumSingletonCase else MemberKind.CaseObject
      SumMember(name, sig, kind, c.tpe, Some(item.ref), item.texts, item.discriminatorKey, ta.definition, schema)
    } else if (isSealed(c.tpe)) {
      val ta     = typeAnnotations(c.tpe)
      val nested = Sum(c.tpe, SumKind.Sealed, sealedChildren(c.tpe).map(member(_, root, ctx)), ta.discriminator, ta.typeHint, hasFlag[json.schema.Flag.EnumsAsOneOf])
      SumMember(name, sig, MemberKind.NestedFamily, c.tpe, None, ta.texts, ta.discriminatorKey, ta.definition, nested)
    } else if (classSymbol(c.tpe).flags.is(Flags.Case)) {
      val ta      = typeAnnotations(c.tpe)
      val derived = lookupImplicit(c.tpe) orElse deriveValueClass(c.tpe, ctx)
      val kind    =
        if (derived.exists(_.isInstanceOf[ValueClass])) MemberKind.ValueClass
        else if (c.sym.flags.is(Flags.Enum)) MemberKind.EnumClassCase
        else MemberKind.CaseClass
      val schema = derived getOrElse product(c.tpe, ctx)
      SumMember(name, sig, kind, c.tpe, None, ta.texts, ta.discriminatorKey, ta.definition, schema)
    } else unsupportedLeaf(c.sym, root.tpe)
  }

  private def isCaseSingleton(sym: Symbol): Boolean =
    sym.flags.is(Flags.Case) || sym.flags.is(Flags.Enum) || (!sym.moduleClass.isNoSymbol && sym.moduleClass.flags.is(Flags.Case))

  private def unsupportedLeaf(sym: Symbol, root: TypeRepr): Nothing =
    report.errorAndAbort(
      s"schema for ${root.show} is not supported: only Scala case classes/objects are supported for Sum Type leaves.\nActual: ${sym.fullName}.\n" +
        s"Please consider using of them for Sum Types with base '${root.show}' or provide a custom implicitly accessible json.Schema for the Sum Type."
    )

  private val DiscriminatorOrder = "@discriminator annotation: please even if using call-by-name, use it in specified order. aka [field, phantom]"

  private def discriminatorField(d: TypeAnnotations.Discriminator): String = d.field match {
    case Arg.Given(f)   => f
    case Arg.Default    => "type"
    case Arg.NonLiteral => report.errorAndAbort(DiscriminatorOrder)
  }

  /** Scala 2's checks: every case class member has the (non-phantom) field; implicit-provided or recursive members can't be discriminated. Singletons are skipped. */
  private def validateDiscriminator(d: TypeAnnotations.Discriminator, leaves: List[SumMember]): Unit = {
    val field   = discriminatorField(d)
    val phantom = d.phantom match {
      case Arg.Given(b)   => b
      case Arg.Default    => true
      case Arg.NonLiteral => report.errorAndAbort(DiscriminatorOrder)
    }
    leaves foreach { m =>
      def validate(n: Node): Unit = n match {
        case o: Obj        => if (!phantom && !o.fields.exists(_.name == field)) report.errorAndAbort(s"Discriminator: Field '$field' is not found in ${m.tpe.show}")
        case v: ValueClass => validate(v.inner)
        case _: Singleton  => ()
        case _             => report.errorAndAbort("Discriminator: Only case classes/objects and value classes are supported candidates for sum type hierarchy")
      }
      validate(m.schema)
    }
  }

  // ---- emit: Scala 2's shapes

  private enum Entry {
    case Simple(schema: Expr[Schema[?]])
    case Merged(key: String, schema: Node, values: List[Expr[Value]])
  }

  def emitSum(s: Sum): Expr[Schema[?]] = {
    val field   = s.discriminator.map(discriminatorField)
    val entries = ListBuffer.empty[Entry]
    s.leaves foreach { m =>
      m.schema match {
        case sg: Singleton if s.enumsAsOneOf => entries += Entry.Simple(emitConstOf(s.tpe, sg.item))
        case sg: Singleton                   =>
          val key = schemaKey(sg.valueSchema)
          entries.collectFirst { case e @ Entry.Merged(`key`, _, _) => e } match {
            case Some(e) =>
              // Scala 2 moves the merged enum to the end
              entries -= e
              entries += e.copy(values = e.values :+ sg.item.value)
            case None => entries += Entry.Merged(key, sg.valueSchema, List(sg.item.value))
          }
        case n => entries += Entry.Simple(emit(shapeMember(m, n, field)))
      }
    }
    val members = entries.toList map {
      case Entry.Simple(e)             => e
      case Entry.Merged(_, schema, vs) => emitEnum(s.tpe, schema, vs)
    }
    emitOneOf(s.tpe, members, field)
  }

  /** A case class / value class / implicit member: texts and key (`withTypeAnnotations`, existing extra first), the discriminator key, then `@definition`. */
  private def shapeMember(m: SumMember, n: Node, field: Option[String]): Node = {
    val typed = n match {
      case _: FromImplicit => n // Scala 2: implicit-provided members get no type annotations
      case _               => n.withExtra(n.extra orElse m.texts.copy(discriminationKey = m.discriminatorKey))
    }
    val keyed = if (field.isDefined) typed.withExtra(typed.extra.copy(discriminationKey = Some(m.discriminatorKey getOrElse m.sig))) else typed
    m.definition match {
      case Some(Arg.Given(label)) =>
        keyed match {
          case d: Def => d.copy(sig = label)
          case other  => Def(m.tpe, label, other)
        }
      case Some(Arg.Default) =>
        keyed match {
          case d: Def => d
          case other  => Def(m.tpe, m.sig, other)
        }
      case _ => keyed
    }
  }

  // ---- sealed families (shared with Enums)

  /** A direct child of a sealed type. A singleton (case object, Scala 3 enum singleton case) is its term symbol and `TermRef`; a class has its type arguments bound from the parent's. */
  final case class SealedChild(sym: Symbol, tpe: TypeRepr, singleton: Boolean)

  /** Not for the library's sealed tuple classes (`Tuple`, `NonEmptyTuple`, `*:`): they have no subclasses to derive from (see [[Tuples]]). */
  def isSealed(tpe: TypeRepr): Boolean = {
    val s = classSymbol(tpe)
    s.isClassDef && s.flags.is(Flags.Sealed) && !isStdTuple(tpe)
  }

  /** The direct children of a sealed type, in declaration order.
    *
    * Scala 3 lists a subclass that is local to a block or anonymous (`new Root {}`, the `sealed abstract case class X private (..)` + `new X(v) {}` smart-constructor idiom) as the sealed class
    * ITSELF, so it can't be recovered: that is a compile error (Scala 2 rejects the anonymous ones too, but derives a local case class).
    */
  def sealedChildren(tpe: TypeRepr): List[SealedChild] = classSymbol(tpe).children map { c =>
    if (c == classSymbol(tpe))
      report.errorAndAbort(
        s"schema for ${tpe.show} is not supported: it has a subclass that is local to a block or anonymous (e.g. `new ${c.name} {}`), which Scala 3 can't list. " +
          s"Declare the subclasses at the top level or in an object, or provide an implicit json.Schema for ${tpe.show}"
      )
    else if (c.isTerm) SealedChild(c, c.termRef, singleton = true)
    else if (c.flags.is(Flags.Module)) {
      val m = c.companionModule
      SealedChild(m, m.termRef, singleton = true)
    } else SealedChild(c, childType(c, tpe), singleton = false)
  }

  /** The type of the (possibly generic) child class `cs` of `parent`: its type parameters bound by matching how `cs` extends the parent's class against `parent`'s type arguments (by name, not by
    * position: `case class Impl[B, A](a: A, b: B) extends Base[A, B]` as a `Base[Int, String]` is `Impl[String, Int]`). A type parameter that doesn't occur in the parent stays as it is.
    */
  private def childType(cs: Symbol, parent: TypeRepr): TypeRepr = {
    val tparams = cs.declaredTypes.filter(_.isTypeParam)
    if (tparams.isEmpty) cs.typeRef
    else {
      val self                                                      = cs.typeRef.appliedTo(tparams.map(_.typeRef))
      def unify(p: TypeRepr, a: TypeRepr): List[(Symbol, TypeRepr)] = p match {
        case AppliedType(_, ps) =>
          a match {
            case AppliedType(_, as) if as.size == ps.size => ps.zip(as).flatMap(unify)
            case _                                        => Nil
          }
        case t if tparams.contains(t.typeSymbol) => List(t.typeSymbol -> a)
        case _                                   => Nil
      }
      val bindings   = unify(self.baseType(classSymbol(parent)), parent.dealias)
      val (from, to) = tparams.flatMap(p => bindings.collectFirst { case (`p`, t) => (p, t) }).unzip
      if (from.isEmpty) self else self.substituteTypes(from, to)
    }
  }

  /** A placeholder for families that aren't ported yet. */
  def notYet(tpe: TypeRepr, what: String): Nothing =
    report.errorAndAbort(s"schema for ${tpe.show} is not supported yet on Scala 3: $what")
}
