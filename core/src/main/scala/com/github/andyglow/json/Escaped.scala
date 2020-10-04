package com.github.andyglow.json

object Escaped {

  def apply(x: String): String = {
    val sb = new StringBuilder
    for { ch <- x } {
      val chx = ch.toInt
      require(chx != 0)
      ch match {
        case '\n' => sb append "\\n"
        case '\t' => sb append "\\t"
        case '\r' => sb append "\\r"
        case '\b' => sb append "\\b"
        case '\f' => sb append "\\f"
        case '\\' => sb append "\\\\"
        case '"'  => sb append "\\\""
        case _ =>
          if (chx >= 0x10000) throw new IllegalArgumentException()
          val c = if (chx > 127) "\\u%04x".format(chx) else ch.toString
          sb append c
      }
    }
    sb.toString()
  }
}
