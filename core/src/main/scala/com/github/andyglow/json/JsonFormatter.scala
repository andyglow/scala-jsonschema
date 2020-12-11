package com.github.andyglow.json

import com.github.andyglow.json.Value._

class JsonFormatter(step: Int = 2) {

  def format(value: Value): String = {
    val sb = new StringBuilder
    _format(value, sb)

    sb.toString()
  }

  private def _format(value: Value, sb: StringBuilder, indent: Int = 0): Unit = {
    val i0 = " " * indent
    val i1 = " " * (indent + step)

    value match {
      case obj(fields)  =>
        sb append "{\n"
        var first = true

        for { (name, value) <- fields.toSeq.sortBy(_._1) } {
          if (!first) {
            sb append ",\n"
          } else {
            first = false
          }

          sb append i1
          sb append s""""${Escaped(name)}": """
          _format(value, sb, indent + step)
        }

        sb append "\n"
        sb append i0
        sb append "}"

      case arr(items)   =>
        sb append "[\n"
        var first = true

        for { value <- items } {
          if (!first) {
            sb append ",\n"
          } else {
            first = false
          }

          sb append i1
          _format(value, sb, indent + step)
        }

        sb append "\n"
        sb append i0
        sb append "]"

      case str(x)       => sb append s""""${Escaped(x)}""""
      case _            => sb append value.toString
    }

    ()
  }
}

object JsonFormatter extends JsonFormatter(step = 2)
