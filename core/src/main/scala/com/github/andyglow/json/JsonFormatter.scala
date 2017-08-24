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
        for { (name, value) <- fields} {
          sb append i1
          sb append s""""$name": """
          _format(value, sb, indent + step)
          sb append ",\n"
        }
        sb append i0
        sb append "}"

      case arr(items)   =>
        sb append "[\n"
        for { value <- items} {
          sb append i1
          _format(value, sb, indent + step)
          sb append ",\n"
        }
        sb append i0
        sb append "]"

      case _            =>
        sb append value.toString
    }
  }

}

object JsonFormatter extends JsonFormatter(step = 2)