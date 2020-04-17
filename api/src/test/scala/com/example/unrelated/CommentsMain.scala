package com.example.unrelated

object CommentsMain {

  def main(args: Array[String]): Unit = {
    val s = json.Json.schema[models.Foo]
    println(s)
  }
}
