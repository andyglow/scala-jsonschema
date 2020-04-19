package com.github.andyglow.scaladoc

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec


class SlowParserSpec extends AnyWordSpec {
  import SlowParser._

  "SlowParser" should {

    "handle oneliners" in {
      parse("/** Example scaladoc **/") shouldBe Right(Scaladoc("Example scaladoc"))
      parse("/** Example scaladoc */") shouldBe Right(Scaladoc("Example scaladoc"))
      parse("/** Example scaladoc*/") shouldBe Right(Scaladoc("Example scaladoc"))
      parse("/** Example scaladoc**/") shouldBe Right(Scaladoc("Example scaladoc"))
    }

    "handle indentation" in {
      val bdy = "example scaladoc"
      val expected = Right(Scaladoc(bdy))

      parse(s"/** $bdy*/") shouldBe expected

      parse(
        s"""/** $bdy
           | */""".stripMargin) shouldBe expected

      parse(
        s"""/** $bdy
           |  */""".stripMargin) shouldBe expected

      parse(
        s"""/**        $bdy
           | */""".stripMargin) shouldBe expected

      parse(
        s"""/**
           | * $bdy
           | */""".stripMargin) shouldBe expected

      parse(
        s"""/**
           |  * $bdy
           |  */""".stripMargin) shouldBe expected
    }

    "handle params" in {
      import Scaladoc._
      import Tag._

      parse("/** @param a A Parameter */") shouldBe Right(Scaladoc(List(Param("a", "A Parameter"))))
      parse {
        """/** @param a A
          |  * @param b B
          |  */
          |""".stripMargin
      } shouldBe Right(Scaladoc(List(Param("a", "A"), Param("b", "B"))))
      parse {
        """/** @param a A long
          |  *          long long description
          |  * @param b B short description
          |  */
          |""".stripMargin
      } shouldBe Right(Scaladoc(List(Param("a", "A long long long description"), Param("b", "B short description"))))
    }

    "handle tparams" in {
      import Scaladoc._
      import Tag._

      parse("/** @tparam T Type Parameter */") shouldBe Right(Scaladoc(List(TParam("T", "Type Parameter"))))
      parse {
        """/** @tparam A A
          |  * @tparam B B
          |  */
          |""".stripMargin
      } shouldBe Right(Scaladoc(List(TParam("A", "A"), TParam("B", "B"))))
      parse {
        """/** @tparam A A long
          |  *           long long description
          |  * @tparam B B short description
          |  */
          |""".stripMargin
      } shouldBe Right(Scaladoc(List(TParam("A", "A long long long description"), TParam("B", "B short description"))))
    }
  }
}
