package com.github.andyglow.jsonschema.derivation

/** Resolution: the core design rule. For every type met (Scala 2's `Macroses.resolve`):
  *   1. the type is already being derived (on the stack) -> a `$ref` ([[Recursion]]);
  *   2. `Nothing` -> compile error;
  *   3. an implicit `Schema[T]` -> definition + `$ref`, else an implicit `Predef[T]` -> inlined ([[Implicits]]). Skipped for the root of `derives` and of `DerivedSchema`;
  *   4. otherwise derived from the type's structure by the first family that accepts it ([[families]]);
  *   5. a nested type of a gated kind ([[isGatedKind]]: a case class or a sealed family with one) that gets here is derived implicitly (issue #542). Semi-auto (`Json.schema`, `objectSchema`,
  *      `derives`, `DerivedSchema`): a deprecation warning, or a compile error under `Flag.SemiAuto` ([[semiAutoGate]]). Auto (`Json.auto.derived`, or `Json.auto` imported and available for the type,
  *      [[Implicits.isAutoAvailable]]): derived in place. The cases of a sealed family being derived are derived as part of it, never gated (they don't come through here).
  *
  * Every type is dealiased first (root and nested, like the Scala 2 macro since s2fix): stack entries, `$ref`s and definitions all use the dealiased type, so a root `type IntBox = Box[Int]` works and
  * an implicit `Schema[String]` is the definition `java.lang.String` everywhere. Only `Json.sig` keeps aliases.
  */
trait Resolve { this: Derivation =>
  import q.reflect.*

  /** Resolution context.
    * @param stack
    *   the (dealiased) types being derived, outermost first (for recursion detection)
    */
  final case class Ctx(stack: List[TypeRepr]) {
    def isRoot: Boolean = stack.isEmpty

    /** The context for the parts of `owner` (its fields, members, elements). */
    def descend(owner: TypeRepr): Ctx = Ctx(stack :+ owner.dealias)

    def onStack(tpe: TypeRepr): Option[TypeRepr] = stack.find(_ =:= tpe)
  }
  object Ctx {
    val root: Ctx = Ctx(Nil)
  }

  /** A structural family: derives a node for `tpe`, or `None` if `tpe` isn't of this family. */
  type Family = (TypeRepr, Ctx) => Option[Node]

  /** The structural families, in the order they are tried. The order matters (Scala 2 order, plus `Option` first): e.g. an all-case-object sealed trait is an enum, not a sum; a case class that is a
    * value class is a value class. Each family lives in its own file; to add one, add its method here at the right position.
    */
  protected def families: List[Family] = List(
    deriveTuple,      // Tuples.scala       *: chains longer than 22, and the tuple types of unknown arity (errors); first for the messages
    deriveOption,     // Options.scala      skeleton
    deriveDictionary, // Collections.scala  stream 3
    deriveArray,      // Collections.scala  stream 3
    deriveEnum,       // Enums.scala        stream 2 (all-case-object families, Scala 3 enums, scala.Enumeration, Java enums)
    deriveSum,        // Sums.scala         stream 2 (sealed families, Either)
    deriveProduct,    // Products.scala     skeleton + stream 1 (case classes, tuples)
    deriveValueClass  // ValueClasses.scala skeleton + stream 1
  )

  def resolve(tpe0: TypeRepr, ctx: Ctx): Node = {
    val tpe = normalizeTuple(tpe0.dealias) // `Int *: String *: EmptyTuple` is `(Int, String)` from here on (Tuples.scala)
    ctx.onStack(tpe) match {
      case Some(entry) => onCycle(tpe, entry)
      case None        =>
        // a wildcard can't be searched for (`asType` asserts) or derived: name the type that has it (the owner's own implicit search ran already)
        if (isWildcard(tpe)) notSupported(ctx.stack.lastOption getOrElse tpe, outer(ctx), WildcardReason)
        if (tpe =:= TypeRepr.of[Nothing]) report.errorAndAbort("No json.Schema[_] for Nothing" + pathOf(ctx))
        val searched = if (ctx.isRoot && skipRootSearch) None else lookupImplicit(tpe, ctx)
        searched getOrElse {
          if (!ctx.isRoot) semiAutoGate(tpe, ctx)
          structural(tpe, ctx)
        }
    }
  }

  /** Step 4 only (no implicit search). */
  def structural(tpe: TypeRepr, ctx: Ctx): Node =
    // `Null <: Map`, `Null <: Iterable`: before the families, which would claim it (after the implicit search: a user `Predef[Null]` wins)
    if (tpe =:= TypeRepr.of[Null]) notSupported(tpe, ctx, NullReason)
    else
      families.iterator.map(_(tpe, ctx)).collectFirst { case Some(n) => n } getOrElse {
        // why an implicit for it failed, if it says more than "none found" (e.g. a refined macro's error: Scala 2 reports it instead)
        val why = implicitFailures(tpe).map(e => s"\n  an implicit for it failed: $e").mkString
        notSupported(tpe, ctx, singletonReason(tpe) getOrElse "", why)
      }

  // ---- semi-auto (issue #542)

  /** A nested type of a gated kind without an implicit `Schema` or `Predef`: derived here (auto) or with the phase-1 warning, or the error under `Flag.SemiAuto`. */
  private def semiAutoGate(tpe: TypeRepr, ctx: Ctx): Unit =
    if (mode != Mode.Auto && !isAutoAvailable(tpe) && isGatedKind(tpe)) {
      if (semiAutoNow) semiAutoError(tpe, ctx)
      // the warning only from an expansion that isn't inside an implicit search: `DerivedSchema` always is (typer), an inline given expanded by another derivation's search is too
      else if (mode != Mode.DerivedSchema && !AutoGuard.isSearching && !warned.exists(_._1 =:= tpe)) warned = warned :+ (tpe -> semiAutoWarning(tpe, ctx))
    }

  private var warned: List[(TypeRepr, String)] = Nil

  /** The phase-1 warning of this expansion, one line per type (one diagnostic: the compiler shows only the first of several at the same position). */
  def reportSemiAutoWarnings(): Unit = if (warned.nonEmpty) report.warning(warned.map(_._2).mkString("\n"))

  private lazy val semiAutoNow: Boolean = hasFlag[json.schema.Flag.SemiAuto]

  /** The kinds semi-auto doesn't derive behind the user's back (design "Gated kinds"): non-tuple case classes, and sealed families with a case class case (not `Either`). Value classes, case-object
    * families, Scala 3 enums without a parameterized case, Java enums, `Enumeration`, tuples, `Option`/`Optional`, collections and maps stay native.
    */
  def isGatedKind(tpe: TypeRepr): Boolean = {
    val native = tpe <:< TypeRepr.of[Option[Any]] || tpe <:< TypeRepr.of[Either[Any, Any]] || tpe <:< TypeRepr.of[Iterable[Any]] || defn.isTupleClass(classSymbol(tpe)) || isStdTuple(tpe)
    !native && (isCaseClass(tpe) || (isSealed(tpe) && hasCaseClassCase(classSymbol(tpe))))
  }

  private def hasCaseClassCase(cls: Symbol): Boolean = cls.children exists { c =>
    !c.isTerm && !c.flags.is(Flags.Module) && (c.flags.is(Flags.Case) || (c.flags.is(Flags.Sealed) && c != cls && hasCaseClassCase(c)))
  }

  /** `derives` would only find its own given (rejected anyway). `DerivedSchema` must not find the type's own implicit `Schema`: standing in for it is what the type class is for (e.g.
    * `implicit val s: Schema[X] = implicitly[DerivedSchema[X]].schema`). `objectSchema` does search, like Scala 2: a root with an implicit `Schema` is then a definition, which is an error there.
    * `auto` searches too: a `Predef` or `Schema` of the root wins over auto, and auto's own hole for the root means "derive it" (see [[Implicits]]).
    */
  private def skipRootSearch: Boolean = mode == Mode.Derived || mode == Mode.DerivedSchema

  // ---- helpers for families

  /** The class (or module class) behind `tpe`, looking through aliases. */
  def classSymbol(tpe: TypeRepr): Symbol = tpe.dealias.widen.typeSymbol

  /** `tpe` is a case class (not a case object, not a value class). */
  def isCaseClass(tpe: TypeRepr): Boolean = {
    val s = classSymbol(tpe)
    s.isClassDef && s.flags.is(Flags.Case) && !s.flags.is(Flags.Module) && !isValueClassSymbol(tpe)
  }

  /** `tpe` is a derived value class (`extends AnyVal`, not a primitive). */
  def isValueClassSymbol(tpe: TypeRepr): Boolean = {
    val s = classSymbol(tpe)
    s.isClassDef && tpe.dealias <:< TypeRepr.of[AnyVal] && !defn.ScalaPrimitiveValueClasses.contains(s)
  }
}
