package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.Golden
import json._
import org.scalatest.funsuite.AnyFunSuite

/** The part of [[GroupCSpec]] that needs the collection family: `Option` in a collection (decided: the items are "X or null"). `Either` (decided: keep) is in [[SumsEnumsCharSpec]]. */
class GroupCSumsSpec extends AnyFunSuite {
  import GroupCSpec.Foo
  import Golden.check

  test("nested Option in a collection (decided: X or null)") {
    check("groupC/List[Option[Foo]]", Json.schema[List[Option[Foo]]])
  }
}
