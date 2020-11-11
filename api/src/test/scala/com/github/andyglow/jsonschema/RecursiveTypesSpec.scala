package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import json.Schema
import json.schema.Version
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec._

// comes first because otherwise it fails on directKnownSubclass resolution
object RecursiveTypesSpec {

  final object case1 {
    final case class SList(head: String, tail: Option[SList])
  }

  final object case2 {
    sealed trait Node
    final object Node {
      final case class Element(tag: String, children: List[Node]) extends Node
      final case class Text(content: String) extends Node
    }
  }

  final object case3 {
    final case class LList[T](head: T, tail: Option[LList[T]])
    final case class User(id: String, friends: LList[String])
  }

  def render(s: Schema[_]): Unit = println {
    JsonFormatter format AsValue.schema(s, Version.Draft07("examples"))
  }
}

class RecursiveTypesSpec extends AnyWordSpec {
  import RecursiveTypesSpec._

//  "recursive types" should {
//
//    "be supported" when {
//
//      "1" in {
//        import case1._
//
//        val schema = json.Json.schema[SList]
//
//        render(schema)
//      }
//
//      "11" in {
//        import case1._
//
//        val schema = json.Json.schema[SList]("farmaldegit")
//        println(schema)
//        render(schema)
//      }
//
//      "2" in {
//        import case2._
//
//        val schema = json.Json.schema[Node]
//
//        render(schema)
//
//      }
//
//      "3" in {
//        import case3._
//
//        val schema = json.Json.schema[User]
//
//        render(schema)
//      }
//
//      "31" in {
//        import case3._
//
//        implicit def llist[T](implicit t: Schema[T]): Schema[LList[T]] = json.Json.schema[LList[T]]
//        val schema = json.Json.schema[User]
//
//        render(schema)
//      }
//    }
//  }
}
