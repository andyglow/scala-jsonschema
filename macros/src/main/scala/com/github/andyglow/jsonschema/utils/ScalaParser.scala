package com.github.andyglow.jsonschema.utils

import com.github.andyglow.jsonschema.ScalaParts._

object ScalaParser {

  def parseField(chars: Array[Char], start: Int): Either[String, ParsedParameter] =
    readFieldDefinition(chars, start).right map ParsedParameter.fromString

  def readFieldDefinition(chars: Array[Char], start: Int): Either[String, String] = {
    var ch: Char        = 0
    var i: Int          = start
    var openParens      = 0
    var openBrackets    = 0
    var openSingleQuote = 0
    var openDoubleQuote = 0
    var completed       = false
    val sb              = new StringBuilder
    while (i <= chars.length && !completed) {
      ch = chars(i)
      val notEnclosed =
        openParens == 0 && openBrackets == 0 && openSingleQuote == 0 && openDoubleQuote == 0
      if (ch == ',' && notEnclosed) completed = true
      else if (ch == ')') {
        completed = notEnclosed
        openParens = openParens - 1
      } else if (ch == '(') openParens = openParens + 1
      else if (ch == '[') openBrackets = openBrackets + 1
      else if (ch == ']') openBrackets = openBrackets - 1
      else if (ch == '"') openDoubleQuote = 1 - openDoubleQuote
      else if (ch == '\'') openSingleQuote = 1 - openSingleQuote
      if (!completed) sb append ch
      i = i + 1
    }
    if (!completed) {
      Left(
        s"err: buf=[$sb], par=$openParens, brk=$openBrackets, sq=$openSingleQuote, dq=$openDoubleQuote"
      )
    } else
      Right {
        sb.toString
      }
  }
//
//  def main(args: Array[String]): Unit = {
//    val text =
//      """case class Foo(
//        | a: Option[Int] = None,
//        | b: Int,
//        | c: Either[Int, String] = Left(22),
//        | d: Option[String] = Some("hello, world"),
//        | e: Option[Char] = Some(','),
//        | f: (Int, Int))
//        |""".stripMargin.toCharArray
//    println(readFieldDefinition(text, 17, 200))
//    println(readFieldDefinition(text, 41, 55))
//    println(readFieldDefinition(text, 50, 500))
//    println(readFieldDefinition(text, 86, 500))
//    println(readFieldDefinition(text, 129, 500))
//    println(readFieldDefinition(text, 159, 500))
//  }
//
  def main(args: Array[String]): Unit = {
    val text =
      """case class Foo(
        | a: Option[Int] = None,
        | b: Int,
        | c: Either[Int, String] = Left(22),
        | d: Option[String] = Some("hello, world"),
        | e: Option[Char] = Some(','),
        | f: (Int, Int))
        |""".stripMargin.toCharArray
    println(parseField(text, 17))
    println(parseField(text, 41))
    println(parseField(text, 50))
    println(parseField(text, 86))
    println(parseField(text, 129))
    println(parseField(text, 159))
  }
}
