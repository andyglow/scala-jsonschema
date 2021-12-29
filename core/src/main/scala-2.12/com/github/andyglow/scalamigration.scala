package com.github.andyglow

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

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
      x mapValues f

    def updatedWith(k: K)(op: Option[V] => Option[V]): Map[K, V] = {
      op(x get k) match {
        case Some(v) => x.updated(k, v)
        case None    => x - k
      }
    }
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

  implicit class ListBufferCompanionMigOps(private val lb: ListBuffer.type) extends AnyVal {

    @inline def from[T](elements: TraversableOnce[T]): ListBuffer[T] =
      lb.apply[T](elements.toSeq: _*)
  }
}
