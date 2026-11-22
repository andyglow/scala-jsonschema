package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

/** Keeps `Json.auto.derived` out of the implicit searches of a derivation.
  *
  * With `Json.auto` imported, `auto.derived[N]` is an implicit `Predef[N]` in the *lexical* scope, so Scala 3 finds it before the `Predef`s in implicit scope (`Predef.intS`, a bridge's or cats'
  * instances), and expands it during the search. Auto's own given must never count (owner decision 2026-11-20: nested types are derived like in plain derivation). So while a derivation is searching,
  * a nested `auto.derived` *declines*: its expansion fails, the compiler drops the candidate and goes on with the other implicits (`Predef.intS` for `Int`, nothing for a case class, which the
  * derivation then derives itself). This also keeps nested auto expansions from deriving anything, which would cost time exponential in the depth of the model.
  *
  * INTERNAL API. A `ThreadLocal` counter, because compilers may run in parallel in one JVM (e.g. sbt).
  */
object AutoGuard {

  private val depth = new ThreadLocal[Int] {
    override def initialValue(): Int = 0
  }

  /** Runs an implicit search of a derivation. */
  def searching[A](search: => A): A = {
    depth.set(depth.get + 1)
    try search
    finally depth.set(depth.get - 1)
  }

  /** `true` while some derivation runs an implicit search. */
  def isSearching: Boolean = depth.get > 0

  /** The expansion of a nested `auto.derived`: fails, so the implicit search skips it. */
  def decline(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort("json.Json.auto.derived is not used inside a schema derivation (the derivation derives nested types itself)")
}
