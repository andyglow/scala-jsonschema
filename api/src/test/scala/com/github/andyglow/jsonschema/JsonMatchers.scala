package com.github.andyglow.jsonschema

import com.github.andyglow.json.comparison.Result
import com.github.andyglow.json.{JsonFormatter, Value}
import org.scalatest.matchers.{MatchResult, Matcher}

trait JsonMatchers {

  class Contain(right: Value.obj) extends Matcher[Value.obj] {

    override def apply(left: Value.obj): MatchResult = {
      MatchResult(
        left.contains(right),
        s"${JsonFormatter format left} doesn't contain ${JsonFormatter format right}",
        s"${JsonFormatter format left} contains ${JsonFormatter format right}"
      )
    }
  }

  class BeStructurallyEqual(right: Value) extends Matcher[Value] {

    override def apply(left: Value): MatchResult = {
      left.diff(right) match {
        case Result.Equal =>
          MatchResult(
            matches = true,
            "",
            s"```\n${JsonFormatter format left}\n```\nis structurally equal to\n```\n${JsonFormatter format right}\n```"
          )
        case Result.Different(diffs) =>
          MatchResult(
            matches = false,
            s"```\n${JsonFormatter format left}\n```\nis not structurally equal to\n```\n${JsonFormatter format right}\n```\nDifferences:\n${diffs
                .map(_.str)
                .mkString("- ", "\n- ", "")}",
            ""
          )
      }
    }
  }

  def containJson(right: Value.obj) = new Contain(right)

  def beStructurallyEqualTo(right: Value) = new BeStructurallyEqual(right)
}

object JsonMatchers extends JsonMatchers
