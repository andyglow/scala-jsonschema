package com.github.andyglow.jsonschema.characterization

import com.github.andyglow.jsonschema.harness.{ErrorPhrases, Golden}
import com.github.andyglow.jsonschema.harness.CompileErrors.errorOf
import json._
import json.schema._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

// NOTE: the order of the models matters. On Scala 2 a class WITHOUT a doc comment picks up the nearest preceding comment in
// the file (BACKLOG #5), so undocumented models that must stay undocumented come first.
object ScaladocCharSpec {
  case class HasStale(s: Stale)

  /** Type scaladoc
    *
    * @param a
    *   from scaladoc
    */
  @description("type annotation")
  case class Precedence(@description("from annotation") a: Int, @description("only annotation") b: Int, c: Int)

  /** @param zz
    *   no such field
    */
  case class Stale(a: Int)

  /** Leaked doc
    *
    * @param a
    *   leaked param
    */
  case class Documented(a: Int)

  case class Undocumented(a: Int)
}

/** Characterization of descriptions from scaladoc, annotations and objectSchema config on Scala 2 (inventory F7, F8, D1-D3, A2).
  */
class ScaladocCharSpec extends AnyFunSuite {
  import ScaladocCharSpec._
  import Golden.check

  test("F7: field descriptions: scaladoc @param beats @description; type: scaladoc beats @description") {
    check("char/scaladoc/Precedence", Json.schema[Precedence])
  }

  test("F7: objectSchema config beats scaladoc and @description") {
    check("char/scaladoc/Precedence.objectSchema", Json.objectSchema[Precedence]("a" -> "from config", "b" -> "from config"))
  }

  test("F8: a stale @param fails on the root (Json.schema and objectSchema)") {
    errorOf(assertCompiles("Json.schema[Stale]")) should include(ErrorPhrases.UnknownField + "zz")
    errorOf(assertCompiles("Json.objectSchema[Stale]()")) should include(ErrorPhrases.UnknownField + "zz")
  }

  test("F8: a stale @param of a nested type is ignored") {
    check("char/scaladoc/HasStale", Json.schema[HasStale])
  }

  test("D1: an undocumented class gets no description (Scala 2: it inherits the preceding comment, see @2.12/@2.13)") {
    check("char/scaladoc/Documented", Json.schema[Documented])
    check("char/scaladoc/Undocumented", Json.schema[Undocumented])
  }
}
