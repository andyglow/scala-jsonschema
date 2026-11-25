package com.github.andyglow.jsonschema

/** Issue #542: set while a derivation runs its own implicit searches (`UImplicits.infer`). A `Json.auto.derived[X]` expanded meanwhile gives the placeholder `Exported(AutoHole.hole[X])` instead of
  * deriving `X` by itself: the derivation replaces it with `X`'s schema from its own expansion (one recursion stack). A counter, not `c.openMacros`, which also holds the derivation during its own
  * silent `c.typecheck` and the `c.eval` of default values.
  */
private[jsonschema] object AutoSearch {

  private val depth = new ThreadLocal[Int] {
    override def initialValue(): Int = 0
  }

  def isSearching: Boolean = depth.get > 0

  def searching[T](body: => T): T = {
    depth.set(depth.get + 1)
    try body
    finally depth.set(depth.get - 1)
  }
}
