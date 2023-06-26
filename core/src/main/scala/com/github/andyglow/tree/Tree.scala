package com.github.andyglow.tree

import scala.language.implicitConversions

// this is borrowed from https://github.com/com-lihaoyi/PPrint
sealed trait Tree {

  def rendered: String = {
    new Truncated(
      new Renderer(120, 2).rec(this, 0, 0).iter,
      120,
      500
    ).mkString
  }
}

object Tree {

  case class Apply(prefix: String, body: Iterator[Tree]) extends Tree
  case class Infix(lhs: Tree, op: String, rhs: Tree)     extends Tree
  case class Lit(body: String) extends Tree {
    val hasNewLine = body.exists(c => c == '\n' || c == '\r')
  }
  case class KV(key: String, value: Tree)         extends Tree
  case class Lazy(body0: Ctx => Iterator[String]) extends Tree
  case class Ctx(width: Int, leftOffset: Int, indentCount: Int, indentStep: Int)

  implicit def stringAsLit(x: String): Lit                                  = Lit(x)
  implicit def pairAsKV[T](pair: (String, T))(implicit conv: T => Tree): KV = KV(pair._1, pair._2)
  implicit def mapAsKVIter(pairs: Map[String, Tree]): Iterator[KV]          = pairs.map { case (k, v) => KV(k, v) }.toIterator
}
