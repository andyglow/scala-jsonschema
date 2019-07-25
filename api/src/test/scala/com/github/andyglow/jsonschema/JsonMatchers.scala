package com.github.andyglow.jsonschema

import com.github.andyglow.json.{JsonFormatter, Value}
import org.scalatest.matchers.{MatchResult, Matcher}


trait JsonMatchers {

  class Contain(right: Value.obj) extends Matcher[Value.obj] {

    override def apply(left: Value.obj): MatchResult = {
      MatchResult(
        left.contains(right),
        s"${JsonFormatter format left} doesn't contain ${JsonFormatter format right}",
        s"${JsonFormatter format left} contains ${JsonFormatter format right}")
    }
  }

  def containJson(right: Value.obj) = new Contain(right)
}

object JsonMatchers extends JsonMatchers