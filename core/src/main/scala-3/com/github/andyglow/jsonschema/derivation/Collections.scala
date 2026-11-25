package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema
import json.Schema.`dictionary`.KeyPattern
import json.schema.validation.Instance.`patternProperties`
import json.schema.validation.Magnet

/** Collections and dictionaries (Scala 2's `UArrays`, `UDictionaries`).
  *
  * Most collections of built-ins never get here: `Predef` has `List`/`Set`/`Vector`/`Array`/`Map` instances for elements that have a `Predef`, so they are inlined by the implicit lookup (the core
  * rule). These families handle the rest, e.g. `List[MyCaseClass]`, `Seq[X]`, `Map[K, MyCaseClass]`. Elements and values are resolved with the full rule, so an element with an implicit `Schema` is a
  * definition + `$ref`.
  *
  *   - [[deriveDictionary]]: an `immutable.Map[K, V]` (or a subtype) -> `dictionary[K, V, C]`. Key handling (inventory T10, quirk 13), in Scala 2's order:
  *     1. `K` is the alias `scala.Predef.String` (as written, not dealiased: `java.lang.String` keys get `^[\s\S]*$`, quirk 13), or a case value class over it: no pattern;
  *     1. an implicit `KeyPattern[K]` (`KeyPattern[Inner]` for a case value class `K(v: Inner)`): `patternProperties` with its pattern, read at runtime (built-ins: Byte/Short/Int/Long `^-?[0-9]+$`,
  *        Char `^[\s\S]$`, java.lang.String `^[\s\S]*$`, UUID, Boolean; enumeratum's are sorted);
  *     1. an enum of strings: `^(?:A|B)$` with the names (regex-escaped, `KeyPattern.escape`) in declaration order (not sorted, unlike `KeyPattern.forEnum`);
  *     1. otherwise a compile error naming the key type (TRIAGE-B, s2fix's text). A `collection.Map` that isn't immutable (`mutable.Map`) is an error too.
  *   - [[deriveArray]]: an `Array[E]` or an `Iterable[E]` -> `array[E, C]`, unique iff `<: immutable.Set` (a `mutable.Set` isn't unique). The collection must take its element type as its only type
  *     parameter (`C[E]`); otherwise (`BitSet`, `class Path[A] extends Iterable[String]`) a compile error (TRIAGE-B, s2fix's text). A parameterless alias (`type Users = List[User]`) is dealiased by
  *     [[Resolve.resolve]] first, so it works.
  *
  * The IR keeps the map key's kind ([[KeyKind]], profiledesign fact 6) besides the emitted pattern, for later profiles (e.g. play-json's UUID keys as an array of pairs).
  */
trait Collections { this: Derivation =>
  import q.reflect.*

  /** What a dictionary key is (profiledesign fact 6). Today's output only uses the pattern derived from it. */
  enum KeyKind {

    /** `scala.Predef.String` as written: no pattern. */
    case Str

    /** `java.lang.String` (spelled so), with the `^[\s\S]*$` pattern of its `KeyPattern` (quirk 13). */
    case JavaStr

    /** `Byte`, `Short`, `Int`, `Long`. */
    case Integral

    case Character
    case Uuid

    /** A case value class `K(fieldName: inner)`; the pattern is the inner type's. */
    case ValueClassOf(fieldName: String, inner: KeyKind)

    /** A string enum (sealed family of case objects, `scala.Enumeration`, Scala 3 `enum`, Java enum): its names in declaration order. */
    case StringEnum(names: List[String])

    /** Any other type with a user-provided `KeyPattern`. */
    case Custom
  }

  /** A `Map[K, V]` key: its type, kind and the `KeyPattern` instance found for it (if any). */
  final case class DictKey(tpe: TypeRepr, kind: KeyKind, keyPattern: Option[Term])

  /** `dictionary[K, V, C](value)`. The key's pattern, if any, is a [[Nodes.Validation]] in `extra`, applied by the emitter (Scala 2's `.withValidation(patternProperties := ..)`). */
  final case class Dict(tpe: TypeRepr, container: TypeRepr, key: DictKey, valueTpe: TypeRepr, value: Node, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(value)
    def mapChildren(f: Node => Node): Node = copy(value = f(value))
    def emitRaw: Expr[Schema[?]]           = construct(TypeRepr.of[Schema.`dictionary`[Any, Any, Map]], List(key.tpe, valueTpe, container), List(emit(value).asTerm))
  }

  /** `array[E, C](element, unique)`. */
  final case class Arr(tpe: TypeRepr, container: TypeRepr, elementTpe: TypeRepr, element: Node, unique: Boolean, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(element)
    def mapChildren(f: Node => Node): Node = copy(element = f(element))
    def emitRaw: Expr[Schema[?]]           =
      construct(TypeRepr.of[Schema.`array`[Any, List]], List(elementTpe, container), List(emit(element).asTerm, Literal(BooleanConstant(unique))))
  }

  /** `inner` (built for another type: a java collection with a canonical container, `java.util.Optional` as an `option`) as a `Schema[tpe]`. The type parameter is phantom. */
  final case class Cast(tpe: TypeRepr, inner: Node, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(inner)
    def mapChildren(f: Node => Node): Node = copy(inner = f(inner))
    def emitRaw: Expr[Schema[?]]           = tpe.asType match {
      case '[t] =>
        val e = emit(inner)
        '{ $e.asInstanceOf[Schema[t]] }
    }
  }

  /** `Companion.apply[targs](args)` of the case class behind `cls`. Built as a tree: the container is a type constructor (`C[_]`, `C[_, _]`), which a quote pattern can't bind. */
  private def construct(cls: TypeRepr, targs: List[TypeRepr], args: List[Term]): Expr[Schema[?]] =
    Select.overloaded(Ref(cls.typeSymbol.companionModule), "apply", targs, args).asExprOf[Schema[?]]

  def deriveDictionary(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    javaArgs(tpe, JMapSymbol) match {
      case Some(List(k, v)) => Some(Cast(tpe, dictionary(tpe, JMapSymbol.typeRef, k, v, ctx)))
      case _                => scalaDictionary(tpe, ctx)
    }

  // `tpe`'s type arguments as a `base` (java.util.Map, java.util.Collection), if it is one. `Null` conforms too, but has no base type: left to the Scala families, as before
  private def javaArgs(tpe: TypeRepr, base: Symbol): Option[List[TypeRepr]] =
    if (tpe.baseClasses.contains(base)) Option(tpe.baseType(base).typeArgs).filter(_.nonEmpty) else None

  private def scalaDictionary(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    if (!(tpe <:< TypeRepr.of[scala.collection.Map[?, ?]])) None
    else if (!(tpe <:< TypeRepr.of[Map[?, ?]]))
      notSupported(tpe, ctx, s"only immutable Maps (scala.collection.immutable.Map) are derived as dictionaries. $ProvideHint")
    else {
      val (container, keyTpe, valueTpe) = tpe match {
        case AppliedType(tc, List(k, v)) if sameArgs(tpe.baseType(MapSymbol), List(k, v)) => (tc, k, v)
        case _                                                                            =>
          val kv = tpe.baseType(MapSymbol).typeArgs.map(showType).mkString(", ")
          notSupported(tpe, ctx, s"a Map must take its key and value types as its only type parameters (like Map[K, V]), and ${showType(tpe)} is a Map[$kv]. $ProvideHint")
      }
      Some(dictionary(tpe, container, keyTpe, valueTpe, ctx))
    }

  /** `dictionary[keyTpe, valueTpe, container]` for the map type `tpe`, with the key's pattern. */
  private def dictionary(tpe: TypeRepr, container: TypeRepr, keyTpe: TypeRepr, valueTpe: TypeRepr, ctx: Ctx): Node = {
    val value  = resolve(valueTpe, ctx.descend(tpe))
    val key    = dictKey(tpe, keyTpe, ctx)
    val isJava = container.typeSymbol == JMapSymbol
    val dict   = Dict(if (isJava) container.appliedTo(List(keyTpe, valueTpe)) else tpe, container, key, valueTpe, value)
    patternOf(key, valueTpe, isJava).fold(dict)(p => dict.withExtra(dict.extra.copy(validations = List(p))))
  }

  def deriveArray(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    javaArgs(tpe, JCollectionSymbol) match {
      // a `java.util.Collection[E]` (List, Set, ArrayList, EnumSet, ...): built with a canonical container, `java.util.Set` or `java.util.Collection`, as a `Schema[tpe]`. Its own
      // constructor can't be the `C[_]` of `array[E, C]` (`EnumSet[E <: Enum[E]]` has a bounded parameter), and it may take no element parameter at all
      case Some(List(e)) =>
        val unique    = tpe.baseClasses.contains(JSetSymbol)
        val container = if (unique) JSetSymbol.typeRef else JCollectionSymbol.typeRef
        Some(Cast(tpe, Arr(container.appliedTo(e), container, e, resolve(e, ctx.descend(tpe)), unique)))
      case _ => scalaArray(tpe, ctx)
    }

  private def scalaArray(tpe: TypeRepr, ctx: Ctx): Option[Node] = {
    val isArray = tpe <:< TypeRepr.of[Array[?]]
    if (!isArray && !(tpe <:< TypeRepr.of[Iterable[?]])) None
    else {
      val element = (if (isArray) tpe else tpe.baseType(IterableSymbol)).typeArgs.headOption getOrElse TypeRepr.of[Any]
      tpe match {
        case AppliedType(tc, List(e)) if e =:= element =>
          Some(Arr(tpe, tc, e, resolve(e, ctx.descend(tpe)), unique = tpe <:< TypeRepr.of[Set[?]]))
        // `List[?]`: the element (`Any` through the base type) isn't the argument, but the reason is the wildcard
        case AppliedType(_, List(_: TypeBounds)) => notSupported(tpe, ctx, WildcardReason)
        case _                                   =>
          notSupported(
            tpe,
            ctx,
            s"a collection must take its element type as its only type parameter (like List[T] or Set[T]), and ${showType(tpe)} is a collection of ${showType(element)}. $ProvideHint"
          )
      }
    }
  }

  private def sameArgs(t: TypeRepr, args: List[TypeRepr]): Boolean =
    t.typeArgs.length == args.length && t.typeArgs.zip(args).forall(_ =:= _)

  private def MapSymbol: Symbol      = TypeRepr.of[Map[Any, Any]].typeSymbol
  private def IterableSymbol: Symbol = TypeRepr.of[Iterable[Any]].typeSymbol

  private def JMapSymbol: Symbol        = TypeRepr.of[java.util.Map[Any, Any]].typeSymbol
  private def JCollectionSymbol: Symbol = TypeRepr.of[java.util.Collection[Any]].typeSymbol
  private def JSetSymbol: Symbol        = TypeRepr.of[java.util.Set[Any]].typeSymbol

  // ---- keys

  /** The key's kind and `KeyPattern`, or the TRIAGE-B compile error. */
  private def dictKey(mapTpe: TypeRepr, keyTpe: TypeRepr, ctx: Ctx): DictKey = {
    def unsupported(why: String): Nothing = {
      val k = showType(keyTpe)
      notSupported(
        mapTpe,
        ctx,
        s"a Map key must be a String, a value class of String, an enum or a type with an implicit json.Schema.dictionary.KeyPattern; " +
          s"the key type $k $why. Provide an implicit KeyPattern[$k], or a json.schema.Predef or json.Schema for the Map"
      )
    }

    // `Map[?, V]`: a wildcard can't be searched for (`keyPattern`'s `asType` asserts)
    if (isWildcard(keyTpe)) notSupported(mapTpe, ctx, WildcardReason)

    // a case value class over `inner`: (field name, inner type as declared)
    val valueClass = if (isValueClassSymbol(keyTpe) && classSymbol(keyTpe).flags.is(Flags.Case)) params(keyTpe) match {
      case p :: Nil => Some((p.name, p.declared))
      case _        => None
    }
    else None

    valueClass match {
      case Some((field, inner)) if isPredefString(inner) => DictKey(keyTpe, KeyKind.ValueClassOf(field, KeyKind.Str), None)
      case _ if isPredefString(keyTpe)                   => DictKey(keyTpe, KeyKind.Str, None)
      case _                                             =>
        val patternTpe = valueClass.fold(keyTpe)(_._2)
        keyPattern(patternTpe) match {
          case Some(kp) =>
            val kind = kindOf(patternTpe)
            DictKey(keyTpe, valueClass.fold(kind)((f, _) => KeyKind.ValueClassOf(f, kind)), Some(kp))
          case None =>
            enumKeyNames(keyTpe, ctx.descend(mapTpe)) match {
              case Some(Right(names)) => DictKey(keyTpe, KeyKind.StringEnum(names), None)
              case Some(Left(why))    => unsupported(why)
              case None               => unsupported("is none of these")
            }
        }
    }
  }

  /** The `patternProperties` validation for a key, if it has a pattern. */
  private def patternOf(key: DictKey, valueTpe: TypeRepr, isJava: Boolean): Option[Validation] = (key.kind, key.keyPattern) match {
    case (_, Some(kp)) =>
      val kpe = kp.asExprOf[KeyPattern[?]]
      Some(keyPatternValidation(key.tpe, valueTpe, '{ $kpe.pattern }, isJava))
    case (KeyKind.StringEnum(names), None) => Some(keyPatternValidation(key.tpe, valueTpe, Expr(names.map(KeyPattern.escape).mkString("^(?:", "|", ")$")), isJava))
    case _                                 => None
  }

  /** `.withValidation(patternProperties := pattern)` on a `dictionary[K, V, _]` (`isJava`: a `dictionary[K, V, java.util.Map]`). */
  private def keyPatternValidation(keyTpe: TypeRepr, valueTpe: TypeRepr, pattern: Expr[String], isJava: Boolean): Validation = new Validation {
    def apply(schema: Expr[Schema[?]]): Expr[Schema[?]] = (keyTpe.asType, valueTpe.asType) match {
      case ('[k], '[v]) if isJava =>
        val s = schema.asExprOf[Schema[java.util.Map[k, v]]]
        '{ $s.withValidation[java.util.Map[k, v], Map[?, ?]](`patternProperties` := $pattern)(using Magnet.javaMapMagnet[k, v]) }
      case ('[k], '[v]) =>
        val s = schema.asExprOf[Schema[Map[k, v]]]
        '{ $s.withValidation[Map[k, v], Map[?, ?]](`patternProperties` := $pattern)(using Magnet.mapMagnet[k, v]) }
    }
  }

  private def keyPattern(tpe: TypeRepr): Option[Term] = tpe.asType match {
    case '[t] =>
      Implicits.search(TypeRepr.of[KeyPattern[t]]) match {
        case s: ImplicitSearchSuccess => Some(s.tree)
        case _                        => None
      }
  }

  /** `tpe` is the alias `scala.Predef.String` as written. Scala 2 compares the key with `typeOf[String]` by `==`, so `java.lang.String` (or another alias of it) isn't (quirk 13). */
  private def isPredefString(tpe: TypeRepr): Boolean = tpe match {
    case t: TypeRef => t.typeSymbol.isAliasType && t.typeSymbol.name == "String" && t.typeSymbol.owner.fullName == "scala.Predef$"
    case _          => false
  }

  private def kindOf(tpe: TypeRepr): KeyKind = {
    val t = tpe.dealias
    if (t =:= TypeRepr.of[java.lang.String]) KeyKind.JavaStr
    else if (t =:= TypeRepr.of[Int] || t =:= TypeRepr.of[Long] || t =:= TypeRepr.of[Short] || t =:= TypeRepr.of[Byte]) KeyKind.Integral
    else if (t =:= TypeRepr.of[java.lang.Integer] || t =:= TypeRepr.of[java.lang.Long] || t =:= TypeRepr.of[java.lang.Short] || t =:= TypeRepr.of[java.lang.Byte]) KeyKind.Integral
    else if (t =:= TypeRepr.of[Char]) KeyKind.Character
    else if (t =:= TypeRepr.of[java.util.UUID]) KeyKind.Uuid
    else KeyKind.Custom
  }
}
