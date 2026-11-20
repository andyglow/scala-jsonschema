package com.github.andyglow.json

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}

import scala.collection.mutable
import scala.util.Try

/** Strict, zero-dependency JSON parser (RFC 8259).
  *
  *   - accepts exactly one JSON value surrounded by optional whitespace (space, tab, CR, LF)
  *   - byte input is decoded as UTF-8; malformed input is rejected
  *   - a leading byte order mark is ignored
  *   - nesting deeper than [[ParseJson.MaxDepth]] is rejected
  *   - for duplicated object keys the last one wins; object field order is preserved
  */
object ParseJson {

  final val MaxDepth = 1000

  def apply(x: String): Try[Value] = Try { new Parser(x).parse() }

  def apply(x: Array[Byte]): Try[Value] = Try { decode(x) } flatMap { apply(_) }

  def apply(x: InputStream): Try[Value] = Try { readAll(x) } flatMap { apply(_) }

  private def readAll(is: InputStream): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](8192)
    var n   = is.read(buf)
    while (n != -1) {
      out.write(buf, 0, n)
      n = is.read(buf)
    }
    out.toByteArray
  }

  private def decode(bytes: Array[Byte]): String = {
    val decoder = StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)

    try decoder.decode(ByteBuffer.wrap(bytes)).toString
    catch {
      case err: CharacterCodingException =>
        throw new JsonParseException("input is not valid UTF-8", 0, err)
    }
  }

  private final class Parser(in: String) {
    private var pos = 0

    def parse(): Value = {
      if (in.startsWith("﻿")) pos = 1
      skipWs()
      val v = readValue(0)
      skipWs()
      if (pos < in.length) fail("unexpected trailing content")
      v
    }

    private def fail(msg: String): Nothing = throw new JsonParseException(msg, pos)

    // `\u0000` is never valid at the positions `peek` is used for, so it doubles as end-of-input marker
    private def peek: Char = if (pos < in.length) in.charAt(pos) else '\u0000'

    private def skipWs(): Unit =
      while (pos < in.length && isWs(in.charAt(pos))) pos += 1

    private def isWs(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    private def expect(c: Char): Unit =
      if (pos < in.length && in.charAt(pos) == c) pos += 1 else fail(s"expected '$c'")

    private def readValue(depth: Int): Value = {
      if (pos >= in.length) fail("unexpected end of input")
      peek match {
        case '{'                                     => readObject(depth + 1)
        case '['                                     => readArray(depth + 1)
        case '"'                                     => Value.str(readString())
        case 't'                                     => readLiteral("true", Value.`true`)
        case 'f'                                     => readLiteral("false", Value.`false`)
        case 'n'                                     => readLiteral("null", Value.`null`)
        case c if c == '-' || (c >= '0' && c <= '9') => readNumber()
        case _                                       => fail("unexpected character")
      }
    }

    private def checkDepth(depth: Int): Unit =
      if (depth > MaxDepth) fail(s"nesting is deeper than $MaxDepth")

    private def readObject(depth: Int): Value = {
      checkDepth(depth)
      expect('{')
      val fields = mutable.LinkedHashMap.empty[String, Value]
      skipWs()
      if (peek == '}') pos += 1
      else {
        var more = true
        while (more) {
          skipWs()
          if (peek != '"') fail("expected object key")
          val k = readString()
          skipWs()
          expect(':')
          skipWs()
          fields.update(k, readValue(depth))
          skipWs()
          peek match {
            case ',' => pos += 1
            case '}' => pos += 1; more = false
            case _   => fail("expected ',' or '}'")
          }
        }
      }
      Value.obj(fields)
    }

    private def readArray(depth: Int): Value = {
      checkDepth(depth)
      expect('[')
      val items = Vector.newBuilder[Value]
      skipWs()
      if (peek == ']') pos += 1
      else {
        var more = true
        while (more) {
          skipWs()
          items += readValue(depth)
          skipWs()
          peek match {
            case ',' => pos += 1
            case ']' => pos += 1; more = false
            case _   => fail("expected ',' or ']'")
          }
        }
      }
      Value.arr(items.result())
    }

    private def readLiteral(text: String, v: Value): Value =
      if (in.startsWith(text, pos)) { pos += text.length; v }
      else fail("unexpected character")

    private def readString(): String = {
      expect('"')
      val sb = new java.lang.StringBuilder
      while (true) {
        if (pos >= in.length) fail("unterminated string")
        val c = in.charAt(pos)
        if (c == '"') {
          pos += 1
          return sb.toString
        } else if (c == '\\') {
          pos += 1
          if (pos >= in.length) fail("unterminated escape sequence")
          in.charAt(pos) match {
            case '"'  => sb.append('"')
            case '\\' => sb.append('\\')
            case '/'  => sb.append('/')
            case 'b'  => sb.append('\b')
            case 'f'  => sb.append('\f')
            case 'n'  => sb.append('\n')
            case 'r'  => sb.append('\r')
            case 't'  => sb.append('\t')
            case 'u'  =>
              if (pos + 4 >= in.length) fail("incomplete unicode escape")
              var code = 0
              var i    = 1
              while (i <= 4) {
                val d = Character.digit(in.charAt(pos + i), 16)
                if (d < 0) fail("invalid unicode escape")
                code = code * 16 + d
                i += 1
              }
              sb.append(code.toChar)
              pos += 4
            case _ => fail("invalid escape sequence")
          }
          pos += 1
        } else if (c < ' ') {
          fail("unescaped control character in string")
        } else {
          sb.append(c)
          pos += 1
        }
      }
      throw new IllegalStateException("unreachable")
    }

    private def readNumber(): Value = {
      val start         = pos
      def digits(): Int = {
        val from = pos
        while (pos < in.length && in.charAt(pos) >= '0' && in.charAt(pos) <= '9') pos += 1
        pos - from
      }

      if (peek == '-') pos += 1
      peek match {
        case '0'                       => pos += 1
        case c if c >= '1' && c <= '9' => digits()
        case _                         => fail("invalid number")
      }
      val isInt = !(peek == '.' || peek == 'e' || peek == 'E')
      if (peek == '.') {
        pos += 1
        if (digits() == 0) fail("invalid number: expected digits after '.'")
      }
      if (peek == 'e' || peek == 'E') {
        pos += 1
        if (peek == '+' || peek == '-') pos += 1
        if (digits() == 0) fail("invalid number: expected exponent digits")
      }

      val text = in.substring(start, pos)
      if (isInt) Value.num(BigInt(text))
      else
        try Value.num(BigDecimal(new java.math.BigDecimal(text)))
        catch { case err: NumberFormatException => throw new JsonParseException(s"number out of range: $text", start, err) }
    }
  }
}

/** @param offset
  *   character offset in the (decoded) input where the problem was detected
  */
final class JsonParseException(message: String, val offset: Int, cause: Throwable = null) extends Exception(s"$message at offset $offset", cause)
