package com.github.andyglow.jsonschema.derivation

/** Resolution: the core design rule. For every type met (Scala 2's `Macroses.resolve`):
  *   1. the type is already being derived (on the stack) -> a `$ref` ([[Recursion]]);
  *   2. `Nothing` -> compile error;
  *   3. an implicit `Schema[T]` -> definition + `$ref`, else an implicit `Predef[T]` -> inlined ([[Implicits]]). Skipped for the root of `derives` and of `DerivedSchema`;
  *   4. otherwise derived from the type's structure by the first family that accepts it ([[families]]).
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
    deriveOption,     // Options.scala      skeleton
    deriveDictionary, // Collections.scala  stream 3
    deriveArray,      // Collections.scala  stream 3
    deriveEnum,       // Enums.scala        stream 2 (all-case-object families, Scala 3 enums, scala.Enumeration, Java enums)
    deriveSum,        // Sums.scala         stream 2 (sealed families, Either)
    deriveProduct,    // Products.scala     skeleton + stream 1 (case classes, tuples)
    deriveValueClass  // ValueClasses.scala skeleton + stream 1
  )

  def resolve(tpe0: TypeRepr, ctx: Ctx): Node = {
    val tpe = tpe0.dealias
    ctx.onStack(tpe) match {
      case Some(entry) => onCycle(tpe, entry)
      case None        =>
        // a wildcard can't be searched for (`asType` asserts) or derived: name the type that has it (the owner's own implicit search ran already)
        if (isWildcard(tpe)) notSupported(ctx.stack.lastOption getOrElse tpe, outer(ctx), WildcardReason)
        if (tpe =:= TypeRepr.of[Nothing]) report.errorAndAbort("No json.Schema[_] for Nothing" + pathOf(ctx))
        val searched = if (ctx.isRoot && skipRootSearch) None else lookupImplicit(tpe)
        searched getOrElse structural(tpe, ctx)
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

  /** `derives` would only find its own given (rejected anyway). `DerivedSchema` must not find the type's own implicit `Schema`: standing in for it is what the type class is for (e.g.
    * `implicit val s: Schema[X] = implicitly[DerivedSchema[X]].schema`). `objectSchema` does search, like Scala 2: a root with an implicit `Schema` is then a definition, which is an error there.
    * `auto` searches too: Scala 3 picks `auto.derived[T]` for every `Predef[T]` (it is in lexical scope, so it shadows the `Predef`s in implicit scope, e.g. `Predef.intS`), and the root search lets
    * those take over (auto declines inside the search through its `AutoGuard.Allowed` parameter, see [[AutoGuard]]).
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
