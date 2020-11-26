package com.github.andyglow.json

import java.io.{ByteArrayInputStream, InputStream}

import scala.collection.mutable
import scala.util.Try


object ParseJson {

  def apply(x: String): Try[Value] = apply(new ByteArrayInputStream(x.getBytes))

  def apply(x: InputStream): Try[Value] = Try {
    var root: MutableValue = null
    var currentName: String = null
    val stack = mutable.Stack[MutableValue]()

    def handleValue(v: MutableValue): Unit = {
      if (root == null) root = v
      stack.headOption foreach {
        case arr: MutableValue.arr => arr += v
        case obj: MutableValue.obj if currentName != null => obj.update(currentName, v)
        case _ =>
      }
    }

    val handler = new JsonHandler {

      override def start(): Unit = {}

      override def end(): Unit = {}

      override def objectStart(): Unit = {
        val o = MutableValue.obj()
        if (root == null) root = o
        handleValue(o)
        stack.push(o)
        currentName = null
      }

      override def objectEnd(): Unit = {
        stack.pop()
        currentName = null
      }

      override def arrayStart(): Unit = {
        val a = MutableValue.arr()
        if (root == null) root = a
        handleValue(a)
        stack.push(a)
        currentName = null
      }

      override def arrayEnd(): Unit = {
        stack.pop()
        currentName = null
      }

      override def name(name: String): Unit = {
        currentName = name
      }

      override def value(value: String): Unit = {
        handleValue(MutableValue.str(value))
        currentName = null
      }

      override def value(value: Int): Unit = {
        handleValue(MutableValue.int(value))
        currentName = null
      }

      override def value(value: Long): Unit = {
        handleValue(MutableValue.int(value))
        currentName = null
      }

      override def value(value: java.math.BigInteger): Unit = {
        handleValue(MutableValue.int(value))
        currentName = null
      }

      override def value(value: java.math.BigDecimal): Unit = {
        handleValue(MutableValue.dec(value))
        currentName = null
      }

      override def value(value: Boolean): Unit = {
        handleValue(MutableValue.bool(value))
        currentName = null
      }

      override def nullValue(): Unit = {
        handleValue(MutableValue.`null`)
        currentName = null
      }
    }

    val parser = new JsonParser(x, handler)
    parser.parse()

    if (root == null) throw new JsonParseException()
    root.toValue
  }
}
