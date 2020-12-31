package com.github.andyglow.json

import scala.language.implicitConversions

object comparison {

  sealed trait Segment { def str: String }
  object Segment {
    case class ByName(name: String) extends Segment { def str: String = name }
    case class ByIndex(index: Int) extends Segment { def str: String = index.toString }
    implicit def stringToSegment(x: String): Segment = ByName(x)
    implicit def intToSegment(x: Int): Segment = ByIndex(x)
  }

  case class Path(segments: List[Segment]) {
    def /(x: Segment): Path = copy(segments = segments :+ x)
    def str: String = segments.map(_.str).mkString("/")
  }

  object Path {

    val Empty: Path = Path(Nil)

    implicit def stringToPath(x: String): Path = new Path(List(x))
    implicit def intToPath(x: Int): Path = new Path(List(x))
  }

  sealed trait Result {
    def +(diff: Diff): Result.Different
    def ++(res: Result): Result = res match {
      case Result.Equal            => this
      case Result.Different(diffs) => diffs.foldLeft(this) { _ + _ }
    }
  }
  object Result {
    case object Equal extends Result { def +(diff: Diff): Different = Different(diff) }
    case class Different(differences: List[Diff]) extends Result {
      def +(diff: Diff): Different = copy(differences = differences :+ diff)
    }
    object Different {
      def apply(x: Diff, xs: Diff*): Different = Different(x +: xs.toList)
    }
  }

  sealed trait Diff { def str: String }

  object Diff {

    // properties
    case class MissingProperty(path: Path, expected: Value) extends Diff {
      def str = s"Missing Property at [${path.str}]. Expected: $expected"
    }
    case class TypeMismatch(path: Path, expected: String, actual: String) extends Diff {
      def str = s"Type Mismatch at [${path.str}]. Expected: $expected. Actual: $actual"
    }
    case class ValueMismatch[T](path: Path, expected: T, actual: T) extends Diff {
      def str = s"Value Mismatch at [${path.str}]. Expected: $expected. Actual: $actual"
    }

    // array
    case class ArrayLengthMismatch(path: Path, expected: Int, actual: Int) extends Diff {
      def str = s"Array Length Mismatch at [${path.str}]. Expected: $expected. Actual: $actual"
    }
    case class MissingElement(path: Path, expected: Value) extends Diff  {
      def str = s"Missing Element at [${path.str}]. Expected: $expected"
    }

    // FIXME: do we also need redundant?
  }
}
