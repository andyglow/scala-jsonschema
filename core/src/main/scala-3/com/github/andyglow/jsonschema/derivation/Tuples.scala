package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

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
  /** A match type, or a match-type alias applied (`Tuple.Concat[A, B]`: `dealias` doesn't unfold those): the only types `simplified` can turn into a tuple. */
  private def isMatchType(tpe: TypeRepr): Boolean = tpe match {
    case _: MatchType       => true
    case AppliedType(tc, _) => !tc.typeSymbol.isClassDef
    case _                  => false
  }

  private def asTuple(tpe: TypeRepr): (TypeRepr, TupleShape) =
    tupleShape(tpe) match {
      case TupleShape.NotATuple if isMatchType(tpe.dealias) => // only a match type is worth reducing (B10-R7)
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

  /** `T` is a `*:` chain of known arity (for `Json.objectSchema`'s case-class check: the raw `*:` isn't one). */
  def isKnownTuple[T: Type]: Boolean = asTuple(TypeRepr.of[T]) match {
    case (_, TupleShape.Known(_)) => true
    case _                        => false
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
        val names = elems.indices.toList.map(i => s"_${i + 1}")
        // `Json.objectSchema` decorations (root only), checked like a case class's (Products.fieldDescriptions)
        val fromSpec = if (ctx.isRoot) objectSchemaDecorations else Nil
        fromSpec foreach { (k, _) =>
          if (!names.contains(k)) report.errorAndAbort(s"unknown field: $k. ${signature(tpe, dealiased = true)} fields are: ${names.mkString(", ")}")
        }
        val descrs = fromSpec.toMap
        val fields = elems.zip(names) map { (e, name) =>
          val ft               = e.dealias
          val (node, optional) = fieldNode(tpe, ft, ctx)
          Field(name, ft, node, optional, hasDefault = false, default = None, description = descrs.get(name), rwMode = RWMode.ReadWrite, annotations = Nil)
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
