package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

/** Keeps `Json.auto.derived` out of the implicit searches of a derivation.
  *
  * With `Json.auto` imported, `auto.derived[N]` is an implicit `Predef[N]` in the *lexical* scope, so Scala 3 finds it before the `Predef`s in implicit scope (`Predef.intS`, a bridge's or cats'
  * instances). Auto's own given must never count (owner decision 2026-11-20: nested types are derived like in plain derivation). So while a derivation is searching, a nested `auto.derived`
  * *declines*: its implicit parameter [[Allowed]] (a transparent given, expanded during the search in every phase) fails, the compiler drops the candidate and goes on with the other implicits
  * (`Predef.intS` for `Int`, nothing for a case class, which the derivation then derives itself). `auto.derived`'s own expansion declines too, as a second line of defence (when a search does expand
  * it). This also keeps nested auto expansions from deriving anything, which would cost time exponential in the depth of the model.
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

  /** The expansion of [[Allowed.allowed]]. */
  def allowedImpl(using Quotes): Expr[Allowed] =
    if (isSearching) decline else '{ Allowed.instance }

  /** Evidence that `Json.auto.derived` may be used here: its implicit parameter.
    *
    * Declining in `auto.derived`'s own expansion works only when the search expands it, i.e. when the derivation searching runs after typer (a plain inline macro: `Json.schema`, `derives`). A
    * derivation that runs in typer (the transparent `DerivedSchema` implicit of module `derived`) gets back the *unexpanded* `auto.derived[Int]`, rejects it ([[Implicits.containsAuto]]) and never
    * reaches `Predef.intS`. The search does resolve `auto.derived`'s implicit parameters, though, and [[allowed]] is transparent, so it is expanded right there, in every phase: it declines while a
    * derivation searches, the parameter is not found, and the candidate is dropped. `auto.derived` itself stays non-transparent, so a type it can't derive is still a compile error that says why
    * (Scala 2 parity: auto is a blackbox macro there).
    */
  final class Allowed private[derivation] ()
  object Allowed {
    val instance: Allowed = new Allowed

    transparent inline given allowed: Allowed = ${ allowedImpl }
  }
}
