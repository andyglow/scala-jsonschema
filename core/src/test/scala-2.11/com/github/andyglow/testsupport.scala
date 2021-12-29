package com.github.andyglow

import com.github.andyglow.scalamigration._
import scala.util.Try
import org.scalatest.matchers.should.Matchers.fail

object testsupport {

  implicit class TestTryOps[T](private val t: Try[T]) extends AnyVal {

    def value: T = t.fold(err => fail("", err), identity)
  }
}
