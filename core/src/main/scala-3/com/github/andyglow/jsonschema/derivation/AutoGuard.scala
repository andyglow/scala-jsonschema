package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

/** Keeps `Json.auto.derived` from deriving anything inside a derivation's implicit searches (issue #542).
  *
  * With `Json.auto` imported, the bridge `Predef.importedPredef` reaches `auto.derived[X]` for every `Predef[X]` without a better instance, also for the element of a `Predef` constructor
  * (`listS(importedPredef(auto.derived[Node]))` for a `Node(children: List[Node])`). If that `auto.derived[Node]` derived `Node` by itself, it would loop, and cost time exponential in the depth of
  * the model. So while a derivation searches, `auto.derived[X]` expands to a cheap placeholder, `Exported(AutoHole.hole[X])`, a ''hole'', which the derivation replaces with `X`'s schema from its own
  * expansion (see [[Implicits]]). A derivation that runs in typer (the transparent `DerivedSchema` of module `derived`) gets the unexpanded `auto.derived[X](..)` back instead, a hole too.
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

  /** The expansion of [[Allowed.allowed]]. */
  def allowedImpl(using Quotes): Expr[Allowed] = '{ Allowed.instance }

  /** `Json.auto.derived`'s implicit parameter, always available. It declined while a derivation searched up to 0.9 (auto's own given didn't count then); since issue #542 a search gets a hole instead.
    * Kept so that `auto.derived`'s signature stays the same.
    */
  final class Allowed private[derivation] ()
  object Allowed {
    val instance: Allowed = new Allowed

    transparent inline given allowed: Allowed = ${ allowedImpl }
  }
}
