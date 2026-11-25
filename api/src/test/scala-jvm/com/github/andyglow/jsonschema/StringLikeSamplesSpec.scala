package com.github.andyglow.jsonschema

import java.io.File
import java.net.InetAddress
import java.nio.charset.Charset
import java.nio.file.Paths
import java.util.{Currency, Locale}
import java.util.regex.Pattern

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/** JVM only (most of these classes aren't in the Scala.js javalib): the sample texts of `StringLikeSchemaSpec` are what the JDK writes, and the two patterns accept every value the JDK knows. */
class StringLikeSamplesSpec extends AnyFunSuite {
  import StringLikeSchemaSpec._

  test("the sample texts are what the JDK writes (toString, toLanguageTag, getHostAddress, name)") {
    Locale.US.toString shouldBe "en_US"
    Locale.US.toLanguageTag shouldBe "en-US"
    Locale.forLanguageTag("sr-Latn-RS").toString shouldBe "sr_RS_#Latn"
    Locale.ROOT.toString shouldBe ""
    Locale.ROOT.toLanguageTag shouldBe "und"
    Currency.getInstance("USD").toString shouldBe "USD"
    Pattern.compile("^a+\\d$").toString shouldBe "^a+\\d$"
    "^a+\\d$".r.toString shouldBe "^a+\\d$"
    InetAddress.getByName("127.0.0.1").getHostAddress shouldBe "127.0.0.1"
    InetAddress.getByName("::1").getHostAddress shouldBe "0:0:0:0:0:0:0:1"
    InetAddress.getByName("2001:db8::2:1").getHostAddress shouldBe "2001:db8:0:0:0:0:2:1"
    Paths.get("/tmp/a b/c.txt").toString shouldBe "/tmp/a b/c.txt".replace('/', File.separatorChar)
    new File("rel/c.txt").toString shouldBe "rel/c.txt".replace('/', File.separatorChar)
    Charset.forName("windows-1252").name shouldBe "windows-1252"
  }

  test("every ISO 4217 code the JDK knows matches the Currency pattern") {
    val p = Pattern.compile(CurrencyPattern)
    list(Currency.getAvailableCurrencies.iterator()).map(_.getCurrencyCode).filterNot(p.matcher(_).matches) shouldBe empty
  }

  test("every charset name and alias the JDK knows matches the Charset pattern") {
    val p     = Pattern.compile(CharsetPattern)
    val names = list(Charset.availableCharsets.values.iterator()).flatMap(c => c.name :: list(c.aliases.iterator()))
    names.filterNot(p.matcher(_).matches) shouldBe empty
  }

  // no JavaConverters: deprecated on 2.13, the shim differs per version
  private def list[T](it: java.util.Iterator[T]): List[T] = {
    val b = List.newBuilder[T]
    while (it.hasNext) b += it.next()
    b.result()
  }
}
