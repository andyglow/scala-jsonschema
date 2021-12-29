package com.github.andyglow.json

import com.github.andyglow.json.Value._

class JsonFormatter(step: Int = 2, sorted: Boolean = false) {
  import JsonFormatter.NL

  def format(value: Value): String = {
    val sb = new StringBuilder
    _format(value, sb)

    sb.toString()
  }

  private def fields(x: obj): scala.collection.Seq[(String, Value)] =
    if (sorted) x.fields.sortBy(_._1) else x.fields

  private def elements(x: arr): scala.collection.Seq[Value] =
    if (sorted) x.value.sorted else x.value

  private def _format(value: Value, sb: StringBuilder, indent: Int = 0): Unit = {
    val i0 = " " * indent
    val i1 = " " * (indent + step)

    value match {
      case o: obj =>
        sb append "{"
        sb append NL
        var first = true

        for { (name, value) <- fields(o) } {
          if (!first) {
            sb append ","
            sb append NL
          } else {
            first = false
          }

          sb append i1
          sb append s""""${Escaped(name)}": """
          _format(value, sb, indent + step)
        }

        sb append NL
        sb append i0
        sb append "}"

      case a: arr =>
        sb append "["
        sb append NL
        var first = true

        for { value <- elements(a) } {
          if (!first) {
            sb append ","
            sb append NL
          } else {
            first = false
          }

          sb append i1
          _format(value, sb, indent + step)
        }

        sb append NL
        sb append i0
        sb append "]"

      case str(x) => sb append s""""${Escaped(x)}""""
      case _      => sb append value.toString
    }

    ()
  }
}

object JsonFormatter {
  val NL: String = System.lineSeparator()

  private lazy val plainFmt  = new JsonFormatter()
  private lazy val sortedFmt = new JsonFormatter(sorted = true)

  def format(value: Value, sorted: Boolean = false): String = {
    def fmt = if (sorted) sortedFmt else plainFmt
    fmt.format(value)
  }
}
