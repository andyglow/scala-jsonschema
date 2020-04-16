package com.example.unrelated

object CommentsMain {

  /** My perfect class
    *
    * @param a A Param
    * @param b B Param
    */
  case class Foo(a: String, b: Int)

  def main(args: Array[String]): Unit = {
    val s = json.Json.schema[Foo]
    println(s)
  }
}
