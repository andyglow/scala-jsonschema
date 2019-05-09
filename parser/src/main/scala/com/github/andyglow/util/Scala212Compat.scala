package com.github.andyglow.util

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


object Scala212Compat {

  implicit class EitherCompatOps[+L, +R](private val e: Either[L, R]) extends AnyVal {

    def map[RR](f: R => RR): Either[L, RR] = e.right map f

    def flatMap[LL >: L, RR](f: R => Either[LL, RR]): Either[LL, RR] = e.right flatMap f

    def toOption: Option[R] = e.right.toOption

    def toSeq: Seq[R] = e.right.toSeq

    def filterOrElse[LL >: L](p: R => Boolean, zero: => LL): Either[LL, R] = e match {
      case Right(b) if !p(b) => Left(zero)
      case _                 => e
    }
  }

  implicit class TryCompatOps[+T](private val e: Try[T]) extends AnyVal {

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
  }
}