package com.github.andyglow

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal


object scalamigration {

  implicit class MapMigOps[K, V](private val x: Map[K, V]) extends AnyVal {

    @inline def mapV[V2](f: V => V2): Map[K, V2] =
      x mapValues f
  }


  implicit class TryMigOps[+T](private val e: Try[T]) extends AnyVal {

    def fold[U](fa: Throwable => U, fb: T => U): U = e match {
      case Success(b) => fb(b)
      case Failure(a) => fa(a)
    }

    def collect[U](pf: PartialFunction[T, U]): Try[U] = e match {

      case Success(b) =>
        try {
          if (pf isDefinedAt b) Success(pf(b))
          else Failure(new NoSuchElementException("Predicate does not hold for " + b))
        } catch {
          case NonFatal(err) => Failure(err)
        }

      case Failure(_) =>
        e.asInstanceOf[Try[U]]
    }

    def find[U](pf: PartialFunction[T, U]): Try[U] = e collect pf
  }
}