package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{Golden, TriageC}
import json._
import org.scalatest.funsuite.AnyFunSuite

object GroupCSpec {
  case class Foo(a: Int)

  case class HasEither(e: Either[Int, String])
  case class HasTuple(t: (Int, String))
  case class HasOptOpt(o: Option[Option[Int]])
}

/** The former TRIAGE group C. Owner decisions (2026-11-20):
  *   - Option at root/nested: PENDING (to match how the bridged JSON libraries encode it); these goldens pin TODAY's output and are tagged TRIAGE-C until it's decided;
  *   - Either and tuples: keep today's output (pinned);
  *   - covariant implicit pick: moved to group B (exact type match), see TriageBPendingSpec.
  */
class GroupCSpec extends AnyFunSuite {
  import GroupCSpec._
  import Golden.check

  test("top-level Option (pending decision: current behavior)", TriageC) {
    check("groupC/Option[Foo]", Json.schema[Option[Foo]])
    check("groupC/Option[Int]", Json.schema[Option[Int]])
  }

  test("nested Option (pending decision: current behavior)", TriageC) {
    check("groupC/List[Option[Foo]]", Json.schema[List[Option[Foo]]])
    check("groupC/HasOptOpt", Json.schema[HasOptOpt])
  }

  test("Either (decided: keep)") {
    check("groupC/Either[Int,String]", Json.schema[Either[Int, String]])
    check("groupC/HasEither", Json.schema[HasEither])
  }

  test("tuples (decided: keep)") {
    check("groupC/Tuple2[Int,String]", Json.schema[(Int, String)])
    check("groupC/HasTuple", Json.schema[HasTuple])
  }
}
