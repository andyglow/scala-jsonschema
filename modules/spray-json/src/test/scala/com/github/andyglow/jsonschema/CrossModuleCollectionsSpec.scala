package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.collections.CollectionsSpec._
import com.github.andyglow.jsonschema.harness.Golden
import json._
import org.scalatest.funsuite.AnyFunSuite

/** A model compiled in another module (the api tests): on Scala 3 its field types come from TASTy, not from source. Definition names must not depend on that (reviewer4 B1): the golden is the one
  * `CollectionsSpec` checks for the same model derived in its own compilation run. Not about spray-json; it lives here because this module depends on the api tests.
  */
class CrossModuleCollectionsSpec extends AnyFunSuite {

  test("definition names of collections in a model from another module") {
    import UserSchemas._
    Golden.check("collections/HasUserSchemas", Json.schema[HasUserSchemas])
  }

  test("collections and dictionaries of a model from another module") {
    Golden.check("collections/Arrays", Json.schema[Arrays])
    Golden.check("collections/Dicts", Json.schema[Dicts])
    Golden.check("collections/SortedDicts", Json.schema[SortedDicts])
  }
}
