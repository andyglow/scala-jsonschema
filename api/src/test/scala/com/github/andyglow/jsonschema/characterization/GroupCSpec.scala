package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.Golden
import json._
import org.scalatest.funsuite.AnyFunSuite

object GroupCSpec {
  case class Foo(a: Int)

  case class HasTuple(t: (Int, String))
  case class HasOptOpt(o: Option[Option[Int]])
}

/** The former TRIAGE group C. Owner decisions (2026-11-20):
  *   - Option at root/nested: decided 2026-11-21 (the Option standard): every `Option[X]` is "X or null" under the default profile (Neutral), `Option[Option[X]]` flattens; the Legacy goldens keep the
  *     0.7 output (`oneOf(object{value}, enum["None"])`, a field unwraps one layer);
  *   - Either and tuples: keep today's output (pinned). Either and Option in a collection are in [[GroupCSumsSpec]] (they need the sum/collection families);
  *   - covariant implicit pick: moved to group B (exact type match), see TriageBPendingSpec.
  */
class GroupCSpec extends AnyFunSuite {
  import GroupCSpec._
  import Golden.check

  test("top-level Option (decided: X or null)") {
    check("groupC/Option[Foo]", Json.schema[Option[Foo]])
    check("groupC/Option[Int]", Json.schema[Option[Int]])
  }

  test("nested Option in a field (decided: flattened, X or null, not required)") {
    check("groupC/HasOptOpt", Json.schema[HasOptOpt])
  }

  test("tuples (decided: keep)") {
    check("groupC/Tuple2[Int,String]", Json.schema[(Int, String)])
    check("groupC/HasTuple", Json.schema[HasTuple])
  }
}
