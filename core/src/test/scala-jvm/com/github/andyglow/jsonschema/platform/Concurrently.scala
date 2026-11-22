package com.github.andyglow.jsonschema.platform

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/** Runs tasks concurrently and returns their results in order. JVM: Futures on the global pool. Scala.js has one thread and can't block on a Future, so it runs them one after another there. */
object Concurrently {

  def run[A](tasks: List[() => A]): List[A] = Await.result(Future.sequence(tasks.map(t => Future(t()))), 1.minute)
}
