package json

import org.scalatest._
import org.scalatest.Matchers._
import Type._
import com.github.andyglow.jsonschema._

class SchemaSpec extends WordSpec {
  import SchemaSpec._

  "Schema" should {

    "generate schema for case class of primitive fields" in {
      import `object`.Field

      case class Foo(name: String, bar: Int)

      Schema[Foo] shouldEqual Schema(`object`(
        Field("name", Schema(`string`()), required = true),
        Field("bar" , Schema(`integer`), required = true)))
    }

    "generate schema for case class of optional primitive fields" in {
      import `object`.Field

      case class Foo(name: Option[String], bar: Option[Int])

      Schema[Foo] shouldEqual Schema(`object`(
        Field("name", Schema(`string`()), required = false),
        Field("bar" , Schema(`integer`), required = false)))
    }

    "generate schema for case class of primitive fields with default values" in {
      import `object`.Field

      case class Foo(name: String = "xxx", bar: Int = 5)

      Schema[Foo] shouldEqual Schema(`object`(
        Field("name", Schema(`string`()), required = false),
        Field("bar" , Schema(`integer`), required = false)))
    }

    "generate schema for case class using another case class" in {
      import `object`.Field

      case class Foo(name: String, bar: Int)

      case class Bar(foo: Foo)

      Schema[Bar] shouldEqual Schema(`object`(
        Field("foo", Schema(`object`(
          Field("name", Schema(`string`()), required = true),
          Field("bar" , Schema(`integer`), required = true))))))
    }

    "generate schema for case class using collection of string" in {
      import `object`.Field

      case class Bar(foo: Iterable[String])

      Schema[Bar] shouldEqual Schema(`object`(
        Field("foo", Schema(`array`(Schema(`string`()))), required = true)))
    }

    "generate schema for case class using collection of integers" in {
      import `object`.Field

      case class Bar(foo: Iterable[Int])

      Schema[Bar] shouldEqual Schema(`object`(
        Field("foo", Schema(`array`(Schema(`integer`))), required = true)))
    }

    "generate schema for value class" in {
      Schema[Bar] shouldEqual Schema(`string`())
    }

    "uses TypeRegistry. simple" in {

      case class Foo(name: String, bar: Int)

      case class Bar(foo: Foo)

      implicit val typeMap: TypeRegistry.Default = new TypeRegistry.Default()

      val s1 = Schema[Foo]

      val s2 = Schema[Bar]

      typeMap.asMap shouldEqual Map(
        TypeSignature[String] -> Schema(`string`()),
        TypeSignature[Int]    -> Schema(`integer`),
        TypeSignature[Foo]    -> s1.definition,
        TypeSignature[Bar]    -> s2.definition)
    }

    "uses TypeRegistry. generic" in {

      sealed trait Trait

      case class Foo(bar: Int)

      case class Bar[T](foo: T)

      implicit val typeMap: TypeRegistry.Default = new TypeRegistry.Default()

      val s1 = Schema[Foo]

      val s2 = Schema[Bar[Foo]]

      typeMap.asMap shouldEqual Map(
        TypeSignature[Int]      -> Schema(`integer`),
        TypeSignature[Foo]      -> s1.definition,
        TypeSignature[Bar[Foo]] -> s2.definition)
    }
  }

}

object SchemaSpec {

  case class Bar(foo: String) extends AnyVal

}