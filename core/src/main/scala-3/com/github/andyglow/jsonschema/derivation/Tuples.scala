package com.github.andyglow.jsonschema.derivation

/** Scala 3 tuple types spelled as `*:` chains (`Int *: String *: EmptyTuple`, what `Tuple.Concat` / `Tuple.Map` / mirrors produce), issue #467. See `docs/design/467-s3-tuple-cons.md`.
  *
  *   - a chain of known arity 1..22 is normalized to its `TupleN` at the top of [[Resolve.resolve]], so it is the same type as `(A, B)` everywhere: output, implicit search, definition names;
  *   - a longer chain (no `Tuple23` exists) becomes the same `_1 .. _N` object, built here ([[deriveTuple]]);
  *   - `Tuple`, `NonEmptyTuple`, `H *: Tuple`, `H *: T` (unknown arity) and `EmptyTuple` are compile errors. `Tuple`, `NonEmptyTuple` and `*:` are sealed in the library, so [[Sums.isSealed]] keeps
  *     the enum and sum families off them ([[isStdTuple]]): those would derive `enum ["EmptyTuple"]` or fail with "No items defined".
  */
trait Tuples { this: Derivation =>
  import q.reflect.*

  enum TupleShape {
    case Known(elems: List[TypeRepr])
    case Empty
    case Unknown
    case NotATuple
  }

  private lazy val consClass: Symbol     = TypeRepr.of[*:[Any, Tuple]].typeSymbol
  private lazy val tupleClass: Symbol    = TypeRepr.of[Tuple].typeSymbol
  private lazy val nonEmptyClass: Symbol = TypeRepr.of[NonEmptyTuple].typeSymbol
  private lazy val emptyTuple: TypeRepr  = TypeRepr.of[EmptyTuple]

  /** The library's tuple classes: `Tuple`, `NonEmptyTuple`, `*:` (not `TupleN`, not `EmptyTuple`). */
  def isStdTuple(tpe: TypeRepr): Boolean = {
    val s = classSymbol(tpe)
    s == consClass || s == tupleClass || s == nonEmptyClass
  }

  /** The shape of `tpe` as a `*:` chain; a `TupleN` on its own is [[TupleShape.NotATuple]] (the product family's). */
  def tupleShape(tpe: TypeRepr): TupleShape = {
    def isCons(t: TypeRepr) = t match {
      case AppliedType(tc, List(_, _)) => tc.typeSymbol == consClass
      case _                           => false
    }
    // the elements of a chain's tail; `simplified` reduces a match-type tail (`Int *: Tuple.Concat[A, B]` after `memberType`)
    def walk(t0: TypeRepr, acc: List[TypeRepr]): TupleShape = {
      val t = t0.dealias.simplified.dealias
      t match {
        case AppliedType(_, List(h, tail)) if isCons(t)                => walk(tail, h :: acc)
        case _ if t =:= emptyTuple                                     => TupleShape.Known(acc.reverse)
        case AppliedType(tc, args) if defn.isTupleClass(tc.typeSymbol) => TupleShape.Known(acc.reverse ++ args)
        case _                                                         => TupleShape.Unknown
      }
    }
    val t = tpe.dealias
    if (isCons(t)) walk(t, Nil)
    else if (t =:= emptyTuple) TupleShape.Empty
    else if (isStdTuple(t)) TupleShape.Unknown
    else TupleShape.NotATuple
  }

  /** `tpe` itself, or its `simplified` form (a field typed `Tuple.Concat[A, B]`) when only that is a tuple shape. */
  private def asTuple(tpe: TypeRepr): (TypeRepr, TupleShape) =
    tupleShape(tpe) match {
      case TupleShape.NotATuple =>
        val s = tpe.dealias.simplified
        tupleShape(s) match {
          case TupleShape.NotATuple => (tpe, TupleShape.NotATuple)
          case shape                => (s.dealias, shape)
        }
      case shape => (tpe, shape)
    }

  /** A `*:` chain of arity 1..22 as its `TupleN`; any other tuple shape as found (for [[deriveTuple]]); anything else unchanged. */
  def normalizeTuple(tpe: TypeRepr): TypeRepr = asTuple(tpe) match {
    case (_, TupleShape.Known(elems)) if elems.size <= 22 => defn.TupleClass(elems.size).typeRef.appliedTo(elems)
    case (t, _)                                           => t
  }

  /** The type the root's title/description are read from: the `TupleN` of a short chain (not the library's `*:` scaladoc), none for a longer one, else `tpe`. */
  def rootTextsType(tpe: TypeRepr): Option[TypeRepr] = asTuple(tpe) match {
    case (_, TupleShape.Known(elems)) if elems.size > 22 => None
    case (_, TupleShape.Known(_))                        => Some(normalizeTuple(tpe))
    case _                                               => Some(tpe)
  }

  /** What [[normalizeTuple]] leaves: a chain of more than 22 elements, or a tuple type of unknown arity / `EmptyTuple` (errors). */
  def deriveTuple(tpe: TypeRepr, ctx: Ctx): Option[Node] = {
    def where = if (ctx.isRoot) "" else s", ${ctx.show}"
    tupleShape(tpe) match {
      case TupleShape.Known(elems) =>
        val fields = elems.zipWithIndex map { (e, i) =>
          val ft               = e.dealias
          val (node, optional) = fieldNode(tpe, ft, ctx)
          Field(s"_${i + 1}", ft, node, optional, hasDefault = false, default = None, description = None, rwMode = RWMode.ReadWrite, annotations = Nil)
        }
        Some(Obj(tpe, ProductKind.Tuple(fields.size), fields))
      case TupleShape.Unknown =>
        report.errorAndAbort(s"schema for ${tpe.show} is not supported: a tuple type needs a known arity (a TupleN, or a *: chain ending in EmptyTuple)$where")
      case TupleShape.Empty =>
        report.errorAndAbort(s"schema for EmptyTuple is not supported: an empty tuple has no fields$where")
      case TupleShape.NotATuple => None
    }
  }
}
