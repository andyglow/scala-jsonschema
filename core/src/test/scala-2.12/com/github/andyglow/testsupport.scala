package com.github.andyglow

import org.scalatest.matchers.should.Matchers.fail
import scala.util.Try

object testsupport {

  implicit class TestTryOps[T](private val t: Try[T]) extends AnyVal {

    def value: T = t.fold(err => fail("", err), identity)
  }
}
