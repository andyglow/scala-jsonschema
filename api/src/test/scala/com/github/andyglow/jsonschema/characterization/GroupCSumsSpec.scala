package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{Golden, TriageC}
import json._
import org.scalatest.funsuite.AnyFunSuite

object GroupCSumsSpec {
  case class HasEither(e: Either[Int, String])
}

/** The part of [[GroupCSpec]] that needs the sum and collection families: `Either` (decided: keep) and `Option` in a collection (pending, current behavior). */
class GroupCSumsSpec extends AnyFunSuite {
  import GroupCSpec.Foo
  import GroupCSumsSpec._
  import Golden.check

  test("nested Option in a collection (pending decision: current behavior)", TriageC) {
    check("groupC/List[Option[Foo]]", Json.schema[List[Option[Foo]]])
  }

  test("Either (decided: keep)") {
    check("groupC/Either[Int,String]", Json.schema[Either[Int, String]])
    check("groupC/HasEither", Json.schema[HasEither])
  }
}
