package com.example.unrelated

import json.{Json, Schema}
import json.schema.Version.Draft07

object CommentsMain {

  private def printSchema[T](s: Schema[T]): Unit = {
    val str = Json.stringify(s, Draft07("foo-id"))
    println(str)
  }

  def main(args: Array[String]): Unit = {
    val s0 = json.Json.objectSchema[models.Foo](
      "c" -> "C, C, ccc",
      "b" -> "bcd")

    printSchema(s0)

    val s1 = json.Json.objectSchema[models.Bar]()

    printSchema(s1)
  }
}
