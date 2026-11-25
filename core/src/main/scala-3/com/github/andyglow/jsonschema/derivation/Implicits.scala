package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.jsonschema.internal.AutoHole
import json.Schema
import json.schema.{Exported, Predef}

/** Implicit lookup (the core design rule, step 1 and 2): an implicit `Schema[T]` becomes a definition + `$ref`, an implicit `Predef[T]` is inlined.
  *
  * A found implicit is rejected (treated as "not found") when
  *   - it is a self-reference: the val/given whose right-hand side is being expanded, e.g. `implicit val s: Schema[X] = Json.schema[X]` or a `derives` given looking for itself. Detected through the
  *     owner chain of the splice (TRIAGE-A: replaces Scala 2's source-text hack). Without this check, a `given` deadlocks at runtime;
  *   - its type argument isn't exactly `T` (TRIAGE-B, owner decision 2026-11-20): `Schema` is covariant, so an implicit `Schema[Sub]` would otherwise satisfy `Schema[Base]`.
  *
  * `Json.auto` (issue #542): with it imported, the bridge `Predef.importedPredef` reaches `auto.derived[X]`, which, while a derivation searches, is a placeholder ([[AutoGuard]]): a ''hole''.
  *   - A hole for `T` itself (`importedPredef(hole[T])`) means "auto is available for `T`": the found tree is dropped and `T` is derived in place, in this expansion ([[isAutoAvailable]]).
  *   - A hole for another type `X` (`nelSchemaFromPredef(importedPredef(hole[P]))`, a user's `boxPredef(importedPredef(hole[X]))`) is an IR child of the [[Nodes.FromImplicit]], resolved with
  *     `resolve(X, ..)` in this expansion (one recursion stack, so a recursive `X` is a `$ref`) and put in place of the hole at emission ([[Nodes.FromImplicit]] `holes`).
  *   - Core's own collection constructors (`Predef.listS`, `setS`, `vectorS`, `arrayS`, `dictS`) with a hole are dropped too: the native collection node derives the same output as without the import
  *     (also where they differ: a `Map[java.lang.String, X]`'s key pattern).
  *
  * A hole is the expanded `auto.derived[X]` (an `Inlined` whose call is `auto.derived`, or `AutoHole.hole[X]` itself), or, for a derivation that runs in typer (`DerivedSchema`), the unexpanded
  * `auto.derived[X](..)` call.
  */
trait Implicits { this: Derivation =>
  import q.reflect.*

  def lookupImplicit(tpe: TypeRepr, ctx: Ctx): Option[Node] = tpe.asType match {
    case '[t] =>
      def fromImplicit(found: Found, source: ImplicitSource, expr: Expr[Schema[?]]): FromImplicit = {
        // in the same context, not under `tpe`: a recursive element is a `$ref` to its own definition (`Box[Node]` in `Node` -> `ref(Node)`), not to the container's
        val holes = resolveHoles(found.holes, ctx)
        FromImplicit(tpe, expr, source, definitionName(tpe), generic = source == ImplicitSource.UserSchema && isGenericImplicit(found.tree), holes = holes)
      }
      searchAccepted(TypeRepr.of[Schema[t]], TypeRepr.of[Schema[Any]].typeSymbol, tpe) match {
        case Some(f) if f.dropped => None
        case Some(f)              => Some(fromImplicit(f, ImplicitSource.UserSchema, f.tree.asExprOf[Schema[t]]))
        case None                 =>
          searchAccepted(TypeRepr.of[Predef[t]], TypeRepr.of[Predef[Any]].typeSymbol, tpe) match {
            case Some(f) if f.dropped => None
            case Some(f)              =>
              val p = f.tree.asExprOf[Predef[t]]
              Some(fromImplicit(f, ImplicitSource.Predef, '{ $p.schema }))
            case None => None
          }
      }
    case _ => None
  }

  /** The holes' types resolved in `ctx` (one node per distinct type). */
  private def resolveHoles(holes: List[TypeRepr], ctx: Ctx): List[(TypeRepr, Node)] =
    holes.foldLeft(List.empty[(TypeRepr, Node)]) { (acc, x) =>
      if (acc.exists(_._1 =:= x)) acc else acc :+ (x -> resolve(x, ctx))
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

  /** An accepted implicit: its tree (wrapped, see [[searchAccepted]]) and the types of the holes in it. `dropped`: the tree isn't used, the type is derived in place (a hole for the searched type
    * itself, or a core collection constructor with a hole, see the trait's doc).
    */
  final case class Found(tree: Term, holes: List[TypeRepr], dropped: Boolean)

  /** Searches an implicit of type `target` (`TC[tpe]`), and returns it unless it's rejected (see the trait's doc).
    *
    * The found tree is wrapped in `Inlined(None, Nil, _)`, which marks it as call-site code. Without that, when the implicit is the whole expansion (`Json.schema[Foo]` with an imported
    * `Schema[Foo]`), `-Wunused:imports` doesn't see the import as used and warns (a nested use was already fine, being spliced into the outer quote).
    */
  def searchAccepted(target: TypeRepr, tc: Symbol, tpe: TypeRepr): Option[Found] =
    AutoGuard.searching(Implicits.search(target)) match {
      case s: ImplicitSearchSuccess if !isSelfRef(s.tree) && isExact(s.tree, tc, tpe) =>
        val holes = holesIn(s.tree).map(_._2)
        if (holes.isEmpty) Some(Found(Inlined(None, Nil, s.tree), Nil, dropped = false))
        else {
          val forIt = holes.exists(_ =:= tpe)
          if (forIt && !isAutoAvailable(tpe)) autoAvailable = autoAvailable :+ tpe
          Some(Found(Inlined(None, Nil, s.tree), holes, dropped = forIt || isCoreCollection(s.tree)))
        }
      case f: ImplicitSearchFailure => recordFailure(tpe, f); None
      case _                        => None
    }

  // ---- Json.auto's holes (issue #542)

  private var autoAvailable: List[TypeRepr] = Nil

  /** The search for `tpe` found auto for `tpe` itself (see the trait's doc): `tpe` is derived in place, as auto derives it. */
  def isAutoAvailable(tpe: TypeRepr): Boolean = autoAvailable.exists(_ =:= tpe)

  private def isAutoDerived(s: Symbol): Boolean =
    !s.isNoSymbol && s.name == "derived" && s.owner.name.stripSuffix("$") == "auto" && s.owner.owner.name.stripSuffix("$") == "Json"

  private lazy val holeSymbol: Symbol = TypeRepr.of[AutoHole.type].typeSymbol.methodMember("hole").head

  /** How a hole is replaced: `Exported(Predef(x))` for (un)expanded `auto.derived[X]`, `Predef(x)` for a bare `AutoHole.hole[X]`. */
  enum HoleKind {
    case Wrapped, Bare
  }

  /** `t` is a hole: its kind and type. */
  def holeOf(t: Tree): Option[(HoleKind, TypeRepr)] = {
    def targ(fn: Term): Option[TypeRepr] = fn match {
      case Apply(f, _)           => targ(f)
      case TypeApply(_, List(a)) => Some(a.tpe)
      case Inlined(_, Nil, e)    => targ(e)
      case _                     => None
    }
    def arg(fn: Term, what: String): TypeRepr = targ(fn) getOrElse report.errorAndAbort(s"json-schema: can't tell the type of the auto-derivation placeholder ${fn.show} ($what)")
    t match {
      case Inlined(Some(call: Term), _, _) if isAutoDerived(call.symbol) => Some(HoleKind.Wrapped -> arg(call, "an expanded json.Json.auto.derived"))
      case a: Apply if isAutoDerived(a.symbol)                           => Some(HoleKind.Wrapped -> arg(a, "json.Json.auto.derived"))
      case a: TypeApply if a.symbol == holeSymbol                        => Some(HoleKind.Bare -> arg(a, "AutoHole.hole"))
      case _                                                             => None
    }
  }

  /** The holes in `t`, outermost first (a hole isn't searched for holes inside it). */
  def holesIn(t: Tree): List[(Tree, TypeRepr)] = {
    val acc = new TreeAccumulator[List[(Tree, TypeRepr)]] {
      def foldTree(found: List[(Tree, TypeRepr)], tree: Tree)(owner: Symbol): List[(Tree, TypeRepr)] = holeOf(tree) match {
        case Some((_, x)) => found :+ (tree -> x)
        case None         => foldOverTree(found, tree)(owner)
      }
    }
    acc.foldTree(Nil, t)(Symbol.spliceOwner)
  }

  /** `t` with every hole replaced by `x`'s schema, from `schemaOf(x)` (a fresh tree for each). A hole of a type `schemaOf` doesn't know is a compile error naming the type and `what` was found. */
  def fillHoles(t: Term, what: => String, schemaOf: TypeRepr => Option[Expr[Schema[?]]]): Term = {
    val m = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = holeOf(tree) match {
        case Some((kind, x)) =>
          val s = schemaOf(x) getOrElse report.errorAndAbort(
            s"schema for ${showType(x)} can't be put in place of json.Json.auto in the implicit found for $what: ${t.show}. Provide a json.schema.Predef or json.Schema for ${showType(x)}"
          )
          val filled = x.asType match {
            case '[x] =>
              val sx = '{ $s.asInstanceOf[Schema[x]] }
              kind match {
                case HoleKind.Wrapped => '{ Exported[Predef[x]](Predef[x]($sx)) }.asTerm
                case HoleKind.Bare    => '{ Predef[x]($sx) }.asTerm
              }
          }
          filled.changeOwner(owner)
        case None => super.transformTerm(tree)(owner)
      }
    }
    m.transformTerm(t)(Symbol.spliceOwner)
  }

  /** No hole is left in the expansion's output (a compile-time check: a hole would throw at runtime). */
  def checkNoHoles(out: Expr[?]): Unit = holesIn(out.asTerm).headOption foreach { case (_, x) =>
    report.errorAndAbort(s"json-schema: a json.Json.auto placeholder for ${showType(x)} is left in the derived schema. This is a bug, please report it")
  }

  /** The found tree is one of core's own collection constructors (`Predef.listS` .. `dictS`). */
  private def isCoreCollection(t: Term): Boolean = {
    def fn(t: Term): Symbol = t match {
      case Inlined(_, Nil, e) => fn(e)
      case Typed(e, _)        => fn(e)
      case Block(Nil, e)      => fn(e)
      case Apply(f, _)        => fn(f)
      case TypeApply(f, _)    => fn(f)
      case other              => other.symbol
    }
    val s = fn(t)
    !s.isNoSymbol && CoreCollections.contains(s.name) && s.owner == TypeRepr.of[Predef.type].typeSymbol
  }

  private val CoreCollections = Set("listS", "setS", "vectorS", "arrayS", "dictS")

  // ---- why an implicit wasn't found, for the "not supported" error

  private var failures: List[(TypeRepr, String)] = Nil

  /** Keeps the explanation of a failed search when it says more than "there is none": a candidate that failed to expand (a macro that aborted, e.g. refined's
    * `There are no values inside of specified range`), an ambiguity, a divergence. Scala 2 reports those directly; here the derivation goes on (a candidate's failure must not break a type the
    * derivation can derive itself), and [[implicitFailures]] adds them to the error when it can't. Not kept: plain misses (`NoMatchingImplicits`, a candidate whose type doesn't match) and
    * `Json.auto`'s bridge.
    */
  private def recordFailure(tpe: TypeRepr, f: ImplicitSearchFailure): Unit = {
    val e       = f.explanation
    val noMatch = f match { // a match, not isInstanceOf: reflect types are abstract, their TypeTest is what tells them apart
      case _: NoMatchingImplicits => true
      case _                      => false
    }
    // "macro expansion was stopped" alone carries no reason (e.g. auto declining through an implicit parameter it can't get)
    // and the auto bridge's own failures (`importedPredef`, `Exported`: there is no `Json.auto` import)
    val plain = noMatch || e.contains("does not match type") || e.contains("AutoGuard") || e.contains("auto.derived") || e.contains("importedPredef") || e.contains("Exported") ||
      e.trim == "macro expansion was stopped"
    if (!plain && !failures.exists { case (t, m) => m == e && t =:= tpe }) failures = failures :+ (tpe -> e)
    // a candidate that applied, but an implicit it needs is missing (`nelSchemaFromPredef` for a `NonEmptyList[P]` without a `Predef[P]`: the search names `P`), for the semi-auto error
    if (plain && !noMatch && e.startsWith(NoImplicitFor)) {
      val m = NoImplicitFor + e.stripPrefix(NoImplicitFor).trim.replaceAll("^json\\.schema\\.Exported\\[(.*)\\]$", "$1") // the type the instance asked for, not auto's bridge
      if (!misses.exists { case (t, x) => x == m && t =:= tpe }) misses = misses :+ (tpe -> m)
    }
  }

  private val NoImplicitFor = "no implicit values were found that match type "

  private var misses: List[(TypeRepr, String)] = Nil

  /** Why an implicit for `tpe` didn't apply, if the compiler named an implicit that it needs (see `recordFailure`). */
  def missReasons(tpe: TypeRepr): List[String] = misses collect { case (t, m) if t =:= tpe => s"an implicit for it needs one it didn't find: $m" }

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

  /** Is there an implicit `Flag` whose static type is `F`? (Scala 2's `Flag with Flag.EnumsAsOneOf = null` and Scala 3's `given Flag.EnumsAsOneOf = ...` both work.) */
  def hasFlag[F: Type]: Boolean = Implicits.search(TypeRepr.of[json.schema.Flag]) match {
    case s: ImplicitSearchSuccess => s.tree.tpe.widen <:< TypeRepr.of[F]
    case _                        => false
  }
}
