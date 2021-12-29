package com.example.unrelated

import json.schema._

object models {

  /** My perfect class
    *
    * @param a
    *   A Param
    * @param b
    *   B Param
    */
  case class Foo(
    a: String,
    b: Int,
    c: Boolean,
    @description("some description") d: Map[String, String]
  )

  @title("A Bar Title")
  @description("this is Bar. description")
  case class Bar(
    @description("aaa") a: String,
    @description("bbb") b: Int,
    @description("ccc") c: Boolean,
    @description("ddd") d: Map[String, String]
  )
}
