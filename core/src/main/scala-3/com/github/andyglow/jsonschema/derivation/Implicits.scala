package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema
import json.schema.Predef

/** Implicit lookup (the core design rule, step 1 and 2): an implicit `Schema[T]` becomes a definition + `$ref`, an implicit `Predef[T]` is inlined.
  *
  * A found implicit is rejected (treated as "not found") when
  *   - it is a self-reference: the val/given whose right-hand side is being expanded, e.g. `implicit val s: Schema[X] = Json.schema[X]` or a `derives` given looking for itself. Detected through the
  *     owner chain of the splice (TRIAGE-A: replaces Scala 2's source-text hack). Without this check, a `given` deadlocks at runtime;
  *   - its type argument isn't exactly `T` (TRIAGE-B, owner decision 2026-11-20): `Schema` is covariant, so an implicit `Schema[Sub]` would otherwise satisfy `Schema[Base]`;
  *   - it contains `Json.auto.derived` (owner decision 2026-11-20: auto's own given never counts, nested types are derived like in plain derivation). See [[AutoGuard]].
  */
trait Implicits { this: Derivation =>
  import q.reflect.*

  def lookupImplicit(tpe: TypeRepr): Option[Node] = tpe.asType match {
    case '[t] =>
      searchAccepted(TypeRepr.of[Schema[t]], TypeRepr.of[Schema[Any]].typeSymbol, tpe) match {
        case Some(tree) =>
          Some(FromImplicit(tpe, tree.asExprOf[Schema[t]], ImplicitSource.UserSchema, definitionName(tpe), generic = isGenericImplicit(tree)))
        case None =>
          searchAccepted(TypeRepr.of[Predef[t]], TypeRepr.of[Predef[Any]].typeSymbol, tpe) map { tree =>
            val p = tree.asExprOf[Predef[t]]
            FromImplicit(tpe, '{ $p.schema }, ImplicitSource.Predef, definitionName(tpe))
          }
      }
    case _ => None
  }

  /** The compile-time definition name of `tpe`, unless it mentions an abstract type (see [[Nodes.FromImplicit]]). */
  def definitionName(tpe: TypeRepr): Option[String] =
    if (mentionsAbstractType(tpe)) None else Some(signature(tpe, dealiased = true))

  /** `tpe` is, or has as a type argument, a type parameter or an abstract type member: its signature would name the parameter (`pkg.Owner.T`), the same for every instantiation. Opaque types are not
    * abstract here: they have a name of their own.
    */
  def mentionsAbstractType(tpe: TypeRepr): Boolean = tpe.dealias match {
    case AppliedType(tc, args) => mentionsAbstractType(tc) || args.exists(mentionsAbstractType)
    case t: TypeRef            =>
      val s = t.typeSymbol
      (s.isTypeParam || s.isAbstractType) && !s.flags.is(Flags.Opaque)
    case _: ParamRef         => true
    case AnnotatedType(u, _) => mentionsAbstractType(u)
    case AndType(a, b)       => mentionsAbstractType(a) || mentionsAbstractType(b)
    case OrType(a, b)        => mentionsAbstractType(a) || mentionsAbstractType(b)
    case _                   => false
  }

  /** The found implicit is a given/def applied to type arguments (`GRec.derived$Schema[Int](using ..)`): see [[Nodes.FromImplicit]] `generic`. */
  def isGenericImplicit(t: Term): Boolean = t match {
    case Inlined(_, Nil, e) => isGenericImplicit(e)
    case Typed(e, _)        => isGenericImplicit(e)
    case Block(Nil, e)      => isGenericImplicit(e)
    case Apply(f, _)        => isGenericImplicit(f)
    case TypeApply(_, _)    => true
    case _                  => false
  }

  /** Searches an implicit of type `target` (`TC[tpe]`), and returns it unless it's rejected (see the trait's doc).
    *
    * The found tree is wrapped in `Inlined(None, Nil, _)`, which marks it as call-site code. Without that, when the implicit is the whole expansion (`Json.schema[Foo]` with an imported
    * `Schema[Foo]`), `-Wunused:imports` doesn't see the import as used and warns (a nested use was already fine, being spliced into the outer quote).
    */
  def searchAccepted(target: TypeRepr, tc: Symbol, tpe: TypeRepr): Option[Term] =
    AutoGuard.searching(Implicits.search(target)) match {
      case s: ImplicitSearchSuccess if !isSelfRef(s.tree) && isExact(s.tree, tc, tpe) && !containsAuto(s.tree) => Some(Inlined(None, Nil, s.tree))
      case f: ImplicitSearchFailure                                                                            => recordFailure(tpe, f); None
      case _                                                                                                   => None
    }

  // ---- why an implicit wasn't found, for the "not supported" error

  private var failures: List[(TypeRepr, String)] = Nil

  /** Keeps the explanation of a failed search when it says more than "there is none": a candidate that failed to expand (a macro that aborted, e.g. refined's
    * `There are no values inside of specified range`), an ambiguity, a divergence. Scala 2 reports those directly; here the derivation goes on (a candidate's failure must not break a type the
    * derivation can derive itself), and [[implicitFailures]] adds them to the error when it can't. Not kept: plain misses (`NoMatchingImplicits`, a candidate whose type doesn't match) and
    * `Json.auto`'s own decline ([[AutoGuard]]).
    */
  private def recordFailure(tpe: TypeRepr, f: ImplicitSearchFailure): Unit = {
    val e       = f.explanation
    val noMatch = f match { // a match, not isInstanceOf: reflect types are abstract, their TypeTest is what tells them apart
      case _: NoMatchingImplicits => true
      case _                      => false
    }
    val plain = noMatch || e.contains("does not match type") || e.contains("AutoGuard") || e.contains("auto.derived")
    if (!plain && !failures.exists { case (t, m) => m == e && t =:= tpe }) failures = failures :+ (tpe -> e)
  }

  /** The kept explanations of failed searches for `tpe` (see `recordFailure`). */
  def implicitFailures(tpe: TypeRepr): List[String] = failures collect { case (t, m) if t =:= tpe => m }

  /** The symbols we're "inside" of: the val/def/given whose right-hand side contains the expansion. */
  private lazy val enclosingSymbols: List[Symbol] = {
    def loop(s: Symbol, acc: List[Symbol]): List[Symbol] =
      if (s.isNoSymbol || s == defn.RootClass) acc else loop(s.owner, s :: acc)
    loop(Symbol.spliceOwner, Nil)
  }

  def isSelfRef(t: Term): Boolean = {
    def sym(t: Term): Symbol = t match {
      case Inlined(_, Nil, e) => sym(e)
      case Typed(e, _)        => sym(e)
      case Block(Nil, e)      => sym(e)
      case other              => other.symbol
    }
    val s = sym(t)
    !s.isNoSymbol && enclosingSymbols.contains(s)
  }

  /** The found implicit is a `TC[X]` with `X =:= tpe` (no covariant pick). */
  def isExact(t: Term, tc: Symbol, tpe: TypeRepr): Boolean =
    t.tpe.widen.baseType(tc) match {
      case AppliedType(_, List(arg)) => arg =:= tpe
      case _                         => false
    }

  /** The tree refers to `json.Json.auto.derived` (a safety net: while a derivation searches, auto declines to expand, see [[AutoGuard]]). */
  def containsAuto(t: Term): Boolean = {
    def isAuto(s: Symbol): Boolean =
      !s.isNoSymbol && s.name == "derived" && s.owner.name.stripSuffix("$") == "auto" && s.owner.owner.name.stripSuffix("$") == "Json"
    val acc = new TreeAccumulator[Boolean] {
      def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
        found || (tree match {
          case Inlined(Some(call), _, _) if isAuto(call.symbol) => true
          case t: Ref if isAuto(t.symbol)                       => true
          case _                                                => foldOverTree(found, tree)(owner)
        })
    }
    acc.foldTree(false, t)(Symbol.spliceOwner)
  }

  /** Is there an implicit `Flag` whose static type is `F`? (Scala 2's `Flag with Flag.EnumsAsOneOf = null` and Scala 3's `given Flag.EnumsAsOneOf = ...` both work.) */
  def hasFlag[F: Type]: Boolean = Implicits.search(TypeRepr.of[json.schema.Flag]) match {
    case s: ImplicitSearchSuccess => s.tree.tpe.widen <:< TypeRepr.of[F]
    case _                        => false
  }
}
