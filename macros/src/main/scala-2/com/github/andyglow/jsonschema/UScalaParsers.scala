package com.github.andyglow.jsonschema

private[jsonschema] trait UScalaParsers { this: UContext =>
  import c.universe._
  import ScalaParts._

  def parseParameter(sym: TermSymbol): ParsedParameter = {
    utils.ScalaParser.parseField(sym.pos.source.content, sym.pos.start) match {
      case Right(x)  => x
      case Left(err) => c.abort(c.enclosingPosition, err)
    }
  }

  def parseFCQN(x: String): Tree = {
    val path = x.split('.')
    if (path.length == 1)
      Ident(TermName(path.head))
    else
      path.tail.foldLeft[Tree](Ident(TermName(path.head))) { case (acc, x) =>
        Select(acc, TermName(x))
      }
  }

}
