package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.json.{ToValue, Value}
import json.Schema

/** Enums (Scala 2's `UEnums`): a family whose every value is a singleton becomes `enum` (or, with `Flag.EnumsAsOneOf`, a `oneOf` of `const`s). Tried in Scala 2's order:
  *   1. Java enums (`java.time.DayOfWeek`, `TimeUnit`): a string enum of the constant names, in declaration order (bugs-1 decision 3A);
  *   2. `scala.Enumeration` (`X.Value`, `X.type`, an alias of `X.Value`): a string enum of the **val names** in declaration order, not the `Value("name")` strings (quirk 12);
  *   3. sealed families whose leaves (nested sealed families flattened) are all objects, and Scala 3 `enum`s whose cases are all singletons (inventory T6):
  *      - without an implicit `ToValue[Root]`: a string enum of the object names. `@typeHint`s other than `String` are ignored with a warning;
  *      - with one: the values are `ToValue(obj)`, and the value schema comes from the member's `@typeHint`, else the root's. No hint is a compile error, so are members with different schemas (quirk
  *        10);
  *      - with `Flag.EnumsAsOneOf` in scope: `oneOf` of `const`s, each with the member's title, description and `@discriminatorKey` (G1). `@discriminator` is ignored.
  *
  * The IR keeps, per value, its name, kind and a reference to the value (profiledesign fact 2): [[EnumItem]]. Map keys (stream 3) get the names through [[enumKeyNames]].
  */
trait Enums { this: Derivation =>
  import q.reflect.*

  enum EnumSource {
    case SealedFamily, Scala3Enum, Enumeration, JavaEnum
  }

  enum ValueKind {
    case CaseObject, EnumSingletonCase, EnumerationValue, JavaConstant
  }

  /** One enum value.
    * @param name
    *   the object / enum case / val / constant name
    * @param tpe
    *   the singleton type of the value (`Color.Red.type`)
    * @param ref
    *   a reference to the value
    * @param value
    *   the JSON value: `Value.str(name)`, or `ToValue[Root](ref)` if [[fromToValue]]
    * @param texts
    *   the value's own title / description (case objects, enum cases)
    */
  final case class EnumItem(
    name: String,
    sig: String,
    kind: ValueKind,
    tpe: TypeRepr,
    ref: Term,
    value: Expr[Value],
    fromToValue: Boolean,
    texts: Extra,
    discriminatorKey: Option[String],
    typeHint: Option[TypeRepr]
  ) {

    /** The value's string, when it is known at compile time (not for `ToValue`-driven values). */
    def knownString: Option[String] = Option.when(!fromToValue)(name)
  }

  /** @param valueSchema
    *   the schema of the values (`string` unless `ToValue`-driven)
    * @param enumsAsOneOf
    *   `Flag.EnumsAsOneOf` in scope (applies to sealed families and Scala 3 enums only, like Scala 2)
    */
  final case class EnumFamily(tpe: TypeRepr, source: EnumSource, items: List[EnumItem], valueSchema: Node, typeHint: Option[TypeRepr], enumsAsOneOf: Boolean, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(valueSchema)
    def mapChildren(f: Node => Node): Node = copy(valueSchema = f(valueSchema))
    def asOneOf: Boolean                   = enumsAsOneOf && (source == EnumSource.SealedFamily || source == EnumSource.Scala3Enum)
    def emitRaw: Expr[Schema[?]]           =
      if (source == EnumSource.SealedFamily || source == EnumSource.Scala3Enum) emitEnumFamily(this) // a family: shaped by the profile
      else if (asOneOf) emitOneOf(tpe, items.map(emitConst), None)
      else emitEnum(tpe, valueSchema, items.map(_.value))
  }

  /** `string`, the value schema of enums with values known at compile time (Scala 2's `SchemaType.Str(tpe, None)`). */
  final case class Str(tpe: TypeRepr, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = Nil
    def mapChildren(f: Node => Node): Node = this
    def emitRaw: Expr[Schema[?]]           = tpe.asType match {
      case '[t] => '{ Schema.`string`[t](None) }
    }
  }

  def deriveEnum(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    javaEnum(tpe) orElse scalaEnumeration(tpe) orElse singletonFamily(tpe, ctx)

  /** For Map keys (stream 3): `None` if `tpe` isn't an enum; the value names in declaration order for a string enum; `Left(why)` for an enum whose values aren't plain strings. */
  def enumKeyNames(tpe: TypeRepr, ctx: Ctx): Option[Either[String, List[String]]] =
    deriveEnum(tpe.dealias, ctx) collect { case e: EnumFamily =>
      if (e.asOneOf) Left("is an enum, but with Flag.EnumsAsOneOf its members aren't plain strings")
      else if (e.items.exists(i => i.fromToValue && i.typeHint.orElse(e.typeHint).isEmpty)) Left("is an enum of non-string values")
      else
        e.valueSchema match {
          case _: Str => Right(e.items.flatMap(_.knownString))
          case _      => Left("is an enum of non-string values")
        }
    }

  /** The value names of `tpe` if it is a string enum (all-object family, Scala 3 `enum` of singletons, `scala.Enumeration#Value`, Java enum), in declaration order; `None` otherwise (also for an enum
    * whose values aren't plain strings: see [[enumKeyNames]] for why). Kept for stream 3 (reviewer3 N3).
    */
  def enumValueNames(tpe: TypeRepr): Option[List[String]] = enumKeyNames(tpe, Ctx.root).flatMap(_.toOption)

  // ---- emit helpers (shared with Sums)

  def emitEnum(tpe: TypeRepr, schema: Node, values: List[Expr[Value]]): Expr[Schema[?]] = tpe.asType match {
    case '[t] =>
      val s = emit(schema)
      values match {
        case first :: rest => '{ Schema.`enum`.of[t]($s, $first, ${ Varargs(rest) }*) } // `enum.of` keeps the values' order (an ordered Set)
        case Nil           => '{ Schema.`enum`[t]($s, scala.collection.immutable.ListSet.empty) }
      }
  }

  def emitOneOf(tpe: TypeRepr, members: List[Expr[Schema[?]]], discriminatorField: Option[String]): Expr[Schema[?]] = tpe.asType match {
    case '[t] =>
      val ms = Expr.ofList(members)
      val df = Expr(discriminatorField)
      '{ Schema.`oneof`[t](scala.collection.immutable.ListSet.from($ms), $df) } // in order
  }

  /** `const` of an item, with the item's own texts and `@discriminatorKey` (Scala 2's `Const(...).withTypeAnnotations(item)`). */
  def emitConstOf(tpe: TypeRepr, item: EnumItem): Expr[Schema[?]] = tpe.asType match {
    case '[t] =>
      val v = item.value
      withExtra('{ Schema.`const`[t]($v) }, item.texts.copy(discriminationKey = item.discriminatorKey))
  }

  private def emitConst(item: EnumItem): Expr[Schema[?]] = emitConstOf(item.tpe, item)

  // ---- Java enums

  private def javaEnum(tpe: TypeRepr): Option[Node] = {
    val sym = classSymbol(tpe)
    Option
      .when(sym.isClassDef && sym.flags.is(Flags.JavaDefined) && sym.flags.is(Flags.Enum)) {
        val items = javaConstants(sym) map { c =>
          val name = c.name
          val ne   = Expr(name)
          EnumItem(name, signature(c.termRef, dealiased = true), ValueKind.JavaConstant, c.termRef, Ref(c), '{ Value.str($ne) }, false, Extra(), None, None)
        }
        if (items.isEmpty) None else Some(EnumFamily(tpe, EnumSource.JavaEnum, items, Str(tpe), None, enumsAsOneOf = false))
      }
      .flatten
  }

  /** The constants of a Java enum, in declaration order. */
  private def javaConstants(cls: Symbol): List[Symbol] = {
    def isConstant(s: Symbol) = s.isTerm && s.flags.is(Flags.Enum) && s.flags.is(Flags.JavaStatic)
    val declared              = cls.companionModule.moduleClass.declaredFields.filter(isConstant)
    if (declared.nonEmpty) declared
    else cls.children.filter(_.isTerm).sortBy(_.name)
  }

  // ---- scala.Enumeration

  private def scalaEnumeration(tpe: TypeRepr): Option[Node] = {
    val enumerationCls = TypeRepr.of[scala.Enumeration].typeSymbol
    val valueCls       = enumerationCls.typeMember("Value")
    val t              = tpe.dealias
    // `X.Value` (or an alias of it): the prefix is the Enumeration object; `X.type`: the object itself
    val obj = t match {
      case TypeRef(pre, _) if t.baseClasses.contains(valueCls) => Some(pre)
      case x: TermRef                                          => Some(x)
      case _                                                   => None
    }
    obj.filter(_.widen.baseClasses.contains(enumerationCls)) map { o =>
      val members = o.widen.typeSymbol.declaredFields filter { f =>
        o.memberType(f).baseClasses.contains(valueCls)
      }
      val items = members.map(_.name.trim).distinct map { name =>
        val f  = members.find(_.name.trim == name).get
        val ne = Expr(name)
        EnumItem(name, signature(f.termRef, dealiased = true), ValueKind.EnumerationValue, f.termRef, Ref(f), '{ Value.str($ne) }, false, Extra(), None, None)
      }
      EnumFamily(tpe, EnumSource.Enumeration, items, Str(tpe), None, enumsAsOneOf = false)
    }
  }

  // ---- sealed families of objects, Scala 3 enums of singleton cases

  private def singletonFamily(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    if (!isSealed(tpe)) None
    else
      singletonLeaves(tpe) map { leaves =>
        val source    = if (isScala3Enum(tpe)) EnumSource.Scala3Enum else EnumSource.SealedFamily
        val rootHint  = typeAnnotations(tpe).typeHint
        val toValue   = toValueOf(tpe)
        val items     = leaves.map(singletonItem(_, tpe, toValue))
        val schema    = singleValueSchema(tpe, items, rootHint, ctx)
        val effective =
          if (items.exists(_.fromToValue)) schema
          else {
            // all values are names: the schema is a string, whatever the hints say
            def ignored(hint: TypeRepr, of: TypeRepr): Unit =
              if (!(hint =:= TypeRepr.of[String]))
                report.warning(s"@typeHint[${short(hint)}] is ignored for ${showType(of)}, as all family members was derived directly from case objects. Enum Type is String")
            rootHint.foreach(ignored(_, tpe))
            items.foreach(i => i.typeHint.foreach(ignored(_, i.tpe)))
            Str(tpe)
          }
        EnumFamily(tpe, source, items, effective, rootHint, hasFlag[json.schema.Flag.EnumsAsOneOf])
      }

  /** The leaves of a sealed family (nested sealed families flattened) if they are all singletons (objects, case or not, and Scala 3 enum cases), in declaration order. */
  private def singletonLeaves(tpe: TypeRepr): Option[List[SealedChild]] = {
    val perChild = sealedChildren(tpe) map { c =>
      if (c.singleton) Some(List(c))
      else if (isSealed(c.tpe)) singletonLeaves(c.tpe)
      else None
    }
    if (perChild.forall(_.isDefined)) Some(perChild.flatMap(_.get)) else None
  }

  def isScala3Enum(tpe: TypeRepr): Boolean = {
    val s = classSymbol(tpe)
    s.flags.is(Flags.Enum) && !s.flags.is(Flags.JavaDefined)
  }

  /** An implicit `ToValue[Root]` (Scala 2: searched with macros disabled), as a function applying it to a value of the family. */
  def toValueOf(root: TypeRepr): Option[Term => Expr[Value]] = root.asType match {
    case '[r] =>
      Implicits.search(TypeRepr.of[ToValue[r]]) match {
        case s: ImplicitSearchSuccess =>
          val tv = s.tree.asExprOf[ToValue[r]]
          Some(ref => '{ $tv.apply(${ ref.asExprOf[r] }) })
        case _ => None
      }
  }

  /** A singleton leaf as an enum value: its name, or `ToValue[Root](leaf)`. */
  def singletonItem(c: SealedChild, root: TypeRepr, toValue: Option[Term => Expr[Value]]): EnumItem = {
    val sym                  = c.sym
    val ta                   = symbolAnnotations(List(sym, sym.moduleClass).filterNot(_.isNoSymbol).distinct)
    val ref                  = Ref(sym)
    val kind                 = if (sym.flags.is(Flags.Enum)) ValueKind.EnumSingletonCase else ValueKind.CaseObject
    val (value, fromToValue) = toValue match {
      case Some(f) => (f(ref), true)
      case None    =>
        val ne = Expr(sym.name)
        ('{ Value.str($ne) }, false)
    }
    EnumItem(sym.name, signature(c.tpe, dealiased = true), kind, c.tpe, ref, value, fromToValue, ta.texts, ta.discriminatorKey, ta.typeHint)
  }

  /** The schema of an enum value (Scala 2's `EnumFamily.schemas`): the item's `@typeHint`, else the root's, resolved; else `string` for a name; a `ToValue`-driven value without a hint is an error.
    */
  def valueSchemaOf(item: EnumItem, rootHint: Option[TypeRepr], ctx: Ctx): Node =
    item.typeHint.orElse(rootHint) match {
      case Some(h) => resolve(h, ctx)
      case None    =>
        // a ToValue-driven value without a hint: its schema is known only at runtime, from the value (lead decision 2026-11-21; it was a compile error)
        if (item.fromToValue) RuntimeValueSchema(item.tpe, item.value)
        else Str(TypeRepr.of[String])
    }

  /** A ToValue-driven value without a `@typeHint` and no compile-time type: `json.Schema.family.valueSchema(value)` (`string` for a string, else a `const` of it). */
  final case class RuntimeValueSchema(tpe: TypeRepr, value: Expr[Value], extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = Nil
    def mapChildren(f: Node => Node): Node = this
    def emitRaw: Expr[Schema[?]]           = '{ Schema.`family`.valueSchema($value) }
  }

  /** Scala 2's structural equality of schemas, used to group enum values: the generated code. */
  def schemaKey(n: Node): String = emit(n).show

  /** The one value schema of the items (Scala 2's `EnumFamily.singleSchema`); no items or several schemas are errors. */
  private def singleValueSchema(tpe: TypeRepr, items: List[EnumItem], rootHint: Option[TypeRepr], ctx: Ctx): Node = {
    val withSchemas = items.map(i => (valueSchemaOf(i, rootHint, ctx.descend(tpe)), i)).filterNot(_._1.isInstanceOf[RuntimeValueSchema])
    val groups      = withSchemas.groupBy((s, _) => schemaKey(s)).toList.sortBy(_._1)
    groups match {
      case Nil if items.nonEmpty => Str(tpe) // every value is runtime-valued: not used
      case Nil                   => report.errorAndAbort(s"Error inferring schema for enum: ${tpe.show}. No items defined")
      case (_, g) :: Nil         => g.head._1
      case several               =>
        val details = several map { (key, g) =>
          val members = g.map(_._2).sortBy(i => showType(i.tpe)) map { m =>
            s"  - ${showType(m.tpe)}.${m.typeHint.fold("")(h => s" Type Hint: ${short(h)}")}"
          }
          s"- $key:\n${members.mkString("\n")}"
        }
        report.errorAndAbort(s"Error inferring schema for enum: ${tpe.show}. Some family members come with different schemas:\n${details.mkString("\n")}")
    }
  }

  /** A type in messages, as Scala 2 shows it: a singleton is `pkg.Obj.type`. */
  def showType(t: TypeRepr): String = t match {
    case _: TermRef => s"${t.show}.type"
    case _          => t.show
  }

  /** A type as Scala 2's `show` prints it in messages (`Int`, not `scala.Int`). */
  def short(t: TypeRepr): String = t.show(using Printer.TypeReprShortCode)
}
