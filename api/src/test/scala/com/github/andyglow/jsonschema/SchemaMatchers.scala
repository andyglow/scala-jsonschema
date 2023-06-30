package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.SchemaEquality.{Equal, UnEqual}
import json.Schema
import org.scalatest.enablers.Sequencing
import org.scalatest.matchers.{MatchResult, Matcher}

class SchemaMatchers(right: Schema[_], checkOrder: Boolean) extends Matcher[Schema[_]] {

  override def apply(left: Schema[_]): MatchResult = {
    import Schema._, `object`.Field

    val eq = SchemaEquality(checkOrder).compute(left, right)
    val explanation = eq match {
      case Equal         => ""
      case UnEqual(diff) => " due to " + diff.toDebugString
    }

    MatchResult(
      eq == Equal,
      left.toDebugString + " was not equal to " + right.toDebugString + explanation,
      left.toDebugString + " was equal to " + right.toDebugString
    )
  }
}

object SchemaMatchers {

  def matchSchema(x: Schema[_]) = new SchemaMatchers(x, checkOrder = true)

  def unorderedMatchSchema(x: Schema[_]) = new SchemaMatchers(x, checkOrder = false)
}
