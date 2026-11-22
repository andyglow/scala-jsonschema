package com.github.andyglow.json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** Numbers from a `Double` render the same on the JVM and on Scala.js: the JVM's layout (`java.lang.Double.toString`), which these strings are. On the JVM this pins the current output; on Scala.js it
  * checks [[Decimals]], which reproduces it (JavaScript itself prints `16`, `10000000`, `5e-324`).
  */
class DecimalsSpec extends AnyFunSuite {

  private val cases: List[(Double, String)] = List(
    16.0                    -> "16.0",
    100.0                   -> "100.0",
    -2.5                    -> "-2.5",
    0.5                     -> "0.5",
    0.1                     -> "0.1",
    123.456                 -> "123.456",
    0.001                   -> "0.001",
    9999999.0               -> "9999999.0",
    1234567.0               -> "1234567.0",
    1e7                     -> "1.0E+7",
    -1e7                    -> "-1.0E+7",
    1.5e10                  -> "1.5E+10",
    1e21                    -> "1.0E+21",
    1e23                    -> "1.0E+23",
    1e-4                    -> "0.00010",
    1.25e-5                 -> "0.0000125",
    1e-7                    -> "1.0E-7",
    0.0                     -> "0.0",
    -0.0                    -> "0.0",
    Double.MaxValue         -> "1.7976931348623157E+308",
    Double.MinPositiveValue -> "4.9E-324"
  )

  cases.zipWithIndex foreach { case ((d, expected), i) =>
    test(s"#$i: $expected") {
      Value.num(d).toString shouldBe expected
    }
  }

  test("Float, Number") {
    Value.num(1.1f).toString shouldBe "1.100000023841858"
    Value.num(java.lang.Integer.valueOf(16): Number).toString shouldBe "16.0"
    Value.num(java.lang.Double.valueOf(0.25): Number).toString shouldBe "0.25"
  }

  test("integral types keep scale 0") {
    Value.num(16).toString shouldBe "16"
    Value.num(16L).toString shouldBe "16"
    Value.num(BigInt(16)).toString shouldBe "16"
  }

  test("NaN and infinities are rejected, as on the JVM") {
    a[NumberFormatException] should be thrownBy Value.num(Double.NaN)
    a[NumberFormatException] should be thrownBy Value.num(Double.PositiveInfinity)
  }
}
