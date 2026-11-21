package com.github.andyglow.jsonschema.harness

import org.scalatest.Assertions
import org.scalatest.exceptions.TestFailedException

/** Helpers for compile-error tests written with ScalaTest's `assertCompiles("...")` (works on Scala 2 and 3).
  *
  * ScalaTest's failure message is `Expected no compiler error, but got the following type error: "<error>", for code: <code>`, so it echoes the code. Always assert on [[errorOf]] (the error part
  * only), never on the whole message: otherwise `include("Key")` holds just because the code says `EnumKeys`.
  *
  * Caveat (Scala 2): the typecheck behind `assertCompiles` doesn't run RefChecks, so errors from that phase (e.g. forward references) aren't seen.
  */
object CompileErrors extends Assertions {

  /** The compiler error of a failing `assertCompiles(...)`, without ScalaTest's prefix and the echoed code. Fails if it compiles. */
  def errorOf(f: => Any): String = outcome(f) getOrElse fail("expected a compile error, but the code compiles")

  /** `None` if `assertCompiles(...)` passes, otherwise the compiler error (without the echoed code). */
  def outcome(f: => Any): Option[String] =
    try { f; None }
    catch { case e: TestFailedException => Some(strip(e.getMessage)) }

  def strip(msg: String): String = {
    val noCode = msg.lastIndexOf(", for code:") match {
      case -1 => msg
      case i  => msg.substring(0, i)
    }
    val prefix = "error: \""
    val start  = noCode.indexOf(prefix) match {
      case -1 => 0
      case i  => i + prefix.length
    }
    val body = noCode.substring(start)
    if (body.endsWith("\"")) body.dropRight(1) else body
  }
}

/** Key phrases of the macro's own compile errors. These are the contract for the Scala 3 port: the same condition must fail with a message containing the phrase (the rest of the text may differ).
  * Compiler-generated texts (crashes, errors in the generated code) are NOT listed here; tests that pin them switch on the Scala version.
  */
object ErrorPhrases {
  val NotSupported       = "is not supported"
  val NoApply            = "No apply function found for"
  val NoItems            = "No items defined"
  val NoSchemaForNothing = "No json.Schema[_] for Nothing"
  // spelled in two parts on purpose: s3/build's Scala 3 test filter skips files whose text contains the macro call
  // `Json` + `.objectSchema`, and this file doesn't call the macro
  val ObjectSchema        = "Json" + ".objectSchema"
  val ObjectSchemaVC      = ObjectSchema + ": can't be used against value classes"
  val ObjectSchemaNonCC   = ObjectSchema + ": can't be used against non-case classes"
  val ConstantStrings     = "only constant Strings are allowed in Description specification"
  val UnknownField        = "unknown field: "
  val NoToValueForDefault = "Can't infer json value for default value of"
  val DiscriminatorOnly   = "Discriminator: Only case classes/objects and value classes are supported candidates for sum type hierarchy"
  val DiscriminatorOrder  = "@discriminator annotation"

  /** Proposal for the TRIAGE-B "clear compile error" fixes, which don't exist yet: reuse the macro's "not supported" wording and name the offending type. Change it here if the fix words it
    * differently.
    */
  val ClearUnsupported = "not supported"
}
