package com.github.andyglow.jsonschema.platform

/** Runs tasks concurrently and returns their results in order. JVM: Futures on the global pool. Scala.js has one thread and can't block on a Future, so it runs them one after another here. */
object Concurrently {

  def run[A](tasks: List[() => A]): List[A] = tasks.map(_())
}
