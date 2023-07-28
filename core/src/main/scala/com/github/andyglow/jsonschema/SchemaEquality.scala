package com.github.andyglow.jsonschema

import json.Schema
import Schema.`object`
import Schema.`def`
import Schema.`oneof`
import Schema.`allof`
import Schema.`not`
import `object`.Field

import com.github.andyglow.scalamigration._

sealed trait SchemaEquality

object SchemaEquality {
  private type F = Field[_]

  sealed trait Segment
  object Segment {
    case class Field(name: String)      extends Segment
    case class Def(ref: String)         extends Segment
    case class Schema(typeName: String) extends Segment
  }

  type Path = List[Segment]

  case object Equal                    extends SchemaEquality
  case class UnEqual(diff: Difference) extends SchemaEquality

  sealed trait Difference {
    def toDebugString: String
  }

  object Difference {

    case class NotMatchingFieldOrder(path: Path, left: List[String], right: List[String]) extends Difference {
      override def toDebugString: String = "field order mismatch at " + path.mkString(".") + ", Left (" + left.mkString(".") + "), Right(" + right.mkString(".") + ")"
    }

    case class MissingFields(path: Path, leftMisses: Set[F], rightMisses: Set[F]) extends Difference {
      override def toDebugString: String = {
        val sb = new StringBuilder("missed fields at ").append(path.mkString("."))
        if (leftMisses.nonEmpty) sb.append(". Left misses: (").append(leftMisses.map(_.name).mkString(".")).append(")")
        if (rightMisses.nonEmpty) sb.append(". Right misses: (").append(rightMisses.map(_.name).mkString(".")).append(")")
        sb.toString()
      }
    }

    case class MissingVariants(path: Path, leftMisses: Set[Schema[_]], rightMisses: Set[Schema[_]]) extends Difference {
      override def toDebugString: String = {
        val sb = new StringBuilder("missed variants at ").append(path.mkString("."))
        if (leftMisses.nonEmpty) sb.append(". Left misses: (").append(leftMisses.map(_.toString).mkString(".")).append(")")
        if (rightMisses.nonEmpty) sb.append(". Right misses: (").append(rightMisses.map(_.toString).mkString(".")).append(")")
        sb.toString()
      }
    }

    case class DefinitionSignatureMismatch(path: Path, left: String, right: String) extends Difference {
      override def toDebugString: String = "def signature mismatch at " + path.mkString(".")
    }

    case class FieldRequirednessMismatch(path: Path, left: Boolean, right: Boolean) extends Difference {
      override def toDebugString: String = "field requiredness mismatch at " + path.mkString(".")
    }

    case class ClashingSchemas(path: Path, left: Schema[_], right: Schema[_]) extends Difference {
      override def toDebugString: String = "schemas mismatch at " + path.mkString(".")
    }
  }

  import Difference._

  case class Context(checkOrder: Boolean) {
    def compute(l: Schema[_], r: Schema[_]): SchemaEquality = internalCompute(Nil, l, r, ctx = this)
  }

  def apply(checkOrder: Boolean = true): Context = Context(checkOrder)
  def unordered: Context                         = Context(checkOrder = false)
  def ordered: Context                           = Context(checkOrder = true)

  def compute(l: Schema[_], r: Schema[_]): SchemaEquality = ordered.compute(l, r)

  private def internalCompute(path: Path, l: Schema[_], r: Schema[_], ctx: Context): SchemaEquality = (l, r) match {

    case (l: `object`[_], r: `object`[_]) =>
      val res: Either[Difference, SchemaEquality] = for {
        _ <- checkMissingFields(path, l, r)
        _ <- if (ctx.checkOrder) checkMatchingFieldOrder(path, l, r) else Right(())
        _ <- checkFieldAttributes(path, l, r, ctx)
      } yield { Equal }

      res.left.map[SchemaEquality](UnEqual).merge

    case (l: `def`[_], r: `def`[_]) =>
      if (l.sig == r.sig) {
        internalCompute(path :+ Segment.Def(l.sig), l.tpe, r.tpe, ctx)
      } else
        UnEqual(DefinitionSignatureMismatch(path, l.sig, r.sig))

    case (l: `not`[_], r: `not`[_]) =>
      internalCompute(path :+ Segment.Schema("not"), l.tpe, r.tpe, ctx)

    case (l: `oneof`[_], r: `oneof`[_]) =>
      val leftMisses  = r.subTypes -- l.subTypes
      val rightMisses = l.subTypes -- r.subTypes

      if (leftMisses.nonEmpty || rightMisses.nonEmpty) UnEqual(MissingVariants(path :+ Segment.Schema("oneof"), leftMisses, rightMisses)) else Equal

    case (l: `allof`[_], r: `allof`[_]) =>
      val leftMisses  = r.subTypes -- l.subTypes
      val rightMisses = l.subTypes -- r.subTypes

      if (leftMisses.nonEmpty || rightMisses.nonEmpty) UnEqual(MissingVariants(path :+ Segment.Schema("allof"), leftMisses, rightMisses)) else Equal

    case (l, r) =>
      if (l == r) Equal else UnEqual(ClashingSchemas(path, l, r))
  }

  private def checkMissingFields(path: Path, l: `object`[_], r: `object`[_]): Either[MissingFields, Unit] = {
    val lf = l.fields.map(f => f.name -> f).toMap[String, F]
    val rf = r.fields.map(f => f.name -> f).toMap[String, F]

    val leftMisses  = rf.keySet -- lf.keySet
    val rightMisses = lf.keySet -- rf.keySet

    if (leftMisses.nonEmpty || rightMisses.nonEmpty) Left(MissingFields(path, leftMisses.map(rf), rightMisses.map(lf))) else Right(())
  }

  // check attributes
  // - required
  // - tpe (schema)
  private def checkFieldAttributes(path: Path, l: `object`[_], r: `object`[_], ctx: Context): Either[Difference, Unit] = {
    val lf = l.fields.map(f => f.name -> f).toMap[String, F]
    val rf = r.fields.map(f => f.name -> f).toMap[String, F]

    val fields = rf.keySet intersect lf.keySet

    fields.foldLeft[Either[Difference, Unit]](Right(())) {
      case (Left(diff), _) => Left(diff)
      case (Right(_), fn) =>
        val ll = lf(fn)
        val rr = rf(fn)

        if (ll.required != rr.required) Left(FieldRequirednessMismatch(path :+ Segment.Field(fn), ll.required, rr.required))
        else {
          internalCompute(path :+ Segment.Field(fn), ll.tpe, rr.tpe, ctx) match {
            case Equal         => Right(())
            case UnEqual(diff) => Left(diff)
          }
        }
    }
  }

  private def checkMatchingFieldOrder(path: Path, l: `object`[_], r: `object`[_]): Either[NotMatchingFieldOrder, Unit] = {
    val lf = l.fields.map(_.name)
    val rf = r.fields.map(_.name)

    if (lf == rf) Right(()) else Left(NotMatchingFieldOrder(path, lf, rf))
  }
}
