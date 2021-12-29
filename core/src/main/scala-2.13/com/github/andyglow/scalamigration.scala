package com.github.andyglow

import scala.util.Try

object scalamigration {

  implicit class SpecificStringOps(private val x: String) extends AnyVal {

    // in order to not clash with different versions of scala as well as jdk
    @inline def asLines: Iterator[String] = x.linesWithSeparators map { _.stripLineEnd }
  }

  implicit class EitherOps[L, R](private val x: Either[L, R]) extends AnyVal {

    @inline def opt: Option[R] = x.toOption
  }

  implicit class MapMigOps[K, V](private val x: Map[K, V]) extends AnyVal {

    @inline def mapV[V2](f: V => V2): Map[K, V2] =
      x.view.mapValues(f).toMap
  }

  implicit class TryMigOps[+T](private val e: Try[T]) extends AnyVal {

    def find[U](pf: PartialFunction[T, U]): Try[U] = e collect pf
  }

}
