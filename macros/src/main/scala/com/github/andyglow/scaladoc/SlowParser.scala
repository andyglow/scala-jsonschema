package com.github.andyglow.scaladoc


import com.github.andyglow.scalamigration._

class SlowParser {
  import SlowParser._

  def parse(x: String): Either[Err, Scaladoc] = x match {
    case `/**...*/`(body) =>
      var scaladoc = Scaladoc(Nil)
      var scope: Scope = Scope.Description
      var name: Option[String] = None
      val buf = new StringBuilder
      def flush() = {
        scope match {
          case Scope.Description => if (buf.nonEmpty) scaladoc = scaladoc.withDescription(buf.toString)
          case Scope.Param       => val n = name.getOrElse(throw new Exception("no param name")); scaladoc = scaladoc.withParam(n, buf.toString)
          case Scope.TParam      => val n = name.getOrElse(throw new Exception("no tparam name")); scaladoc = scaladoc.withTParam(n, buf.toString)
        }

        buf.setLength(0)
      }

      body.asLines.map(_.trim).filterNot(_.isEmpty) foreach { l =>
        if (l.startsWith("@param")) {
          flush()
          scope = Scope.Param
          // first token
          val _name = l.substring(7).takeWhile(x => !x.isWhitespace)
          buf.append(l.substring(7 + _name.length).trim)
          name = Some(_name)

        } else if (l.startsWith("@tparam")) {
          flush()
          scope = Scope.TParam
          // first token
          val _name = l.substring(8).takeWhile(x => !x.isWhitespace)

          buf.append(l.substring(8 + _name.length).trim)
          name = Some(_name)

        } else {
          if (buf.nonEmpty) buf.append(" ")
          buf.append(l)
        }
      }
      flush()
      Right(scaladoc)
    case _ => Left(NotComment)
  }
}

object SlowParser extends SlowParser {

  sealed trait Scope
  object Scope {
    case object Description extends Scope
    case object Param extends Scope
    case object TParam extends Scope
  }

  sealed trait Err
  case object NotComment extends Err

  object `/**...*/` {

    def unapply(x: String): Option[String] = x.trim match {
        case x if !(x.startsWith("/**")) => None
        case x if !(x.endsWith("*/")) => None
        case x => Some {
          val stripped = {
            val t = x.drop(3).dropRight(2)
            if (t.last == '*') t.dropRight(1) else t
          }
          stripped.asLines.map { l: String =>
            l.dropWhile(c => c.isWhitespace || c == '*')
          }.mkString("\n")
        }
      }
  }
}