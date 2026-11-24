package json.schema

import java.net.{URI, URL}
import java.util.UUID

import json._
import json.Schema._
import json.Schema.`dictionary`.KeyPattern
import json.Schema.`string`.Format
import json.schema.validation.Instance._

import scala.concurrent.duration.Duration

/** The idea behind Predef is a continuation of the idea about exposing schemas that was found in implicit scope as `ref`. Namely, if schema is derived - it will be defined in-place inside of a
  * json-schema, but if it's implicitly available - going to be a `ref`.
  *
  * So Predef changes it by adding another class of implicitly defined schemas which won't become a `ref`
  *
  * @param schema
  * @tparam T
  */
final case class Predef[T](schema: Schema[T]) extends AnyVal

trait LowPriorityPredefs {
//  implicit def iterableS[F[_], T](implicit p: Predef[T], ev0: F[T] <:< Iterable[T]): Predef[F[T]] = Predef(`array`[T, F](p.schema))
}
object Predef extends LowPriorityPredefs {
  implicit val strS: Predef[String] = Predef(`string`[String])
  implicit val chrS: Predef[Char]   = Predef(
    `string`[Char].withValidation(`minLength` := 1, `maxLength` := 1, `pattern` := "^[.\\s]$")
  )
  implicit val jchrS: Predef[Character] = Predef(
    `string`[Character].withValidation(`minLength` := 1, `maxLength` := 1, `pattern` := "^[.\\s]$")
  )
  implicit val boolS: Predef[Boolean]          = Predef(`boolean`)
  implicit val byteS: Predef[Byte]             = Predef(`number`[Byte])
  implicit val shortS: Predef[Short]           = Predef(`number`[Short])
  implicit val intS: Predef[Int]               = Predef(`integer`)
  implicit val doubleS: Predef[Double]         = Predef(`number`[Double])
  implicit val floatS: Predef[Float]           = Predef(`number`[Float])
  implicit val longS: Predef[Long]             = Predef(`number`[Long])
  implicit val bigIntS: Predef[BigInt]         = Predef(`number`[BigInt])
  implicit val bigDecimalS: Predef[BigDecimal] = Predef(`number`[BigDecimal])
  implicit val uuidS: Predef[UUID]             = Predef(`string`[UUID](Format.`uuid`))
  implicit val uriS: Predef[URI]               = Predef(`string`[URI](Format.`uri`))
  implicit val urlS: Predef[URL]               = Predef(`string`[URL](Format.`uri`))
  implicit val juDateS: Predef[java.util.Date] = Predef(
    `string`[java.util.Date](Format.`date-time`)
  )
  implicit val jsqlTimestampS: Predef[java.sql.Timestamp] = Predef(
    `string`[java.sql.Timestamp](Format.`date-time`)
  )
  implicit val instantS: Predef[java.time.Instant] = Predef(
    `string`[java.time.Instant](Format.`date-time`)
  )
  implicit val localDateTimeS: Predef[java.time.LocalDateTime] = Predef(
    `string`[java.time.LocalDateTime](Format.`date-time`)
  )
  implicit val zonedDateTimeS: Predef[java.time.ZonedDateTime] = Predef(
    `string`[java.time.ZonedDateTime](Format.`date-time`)
  )
  implicit val jsqlDateS: Predef[java.sql.Date]        = Predef(`string`[java.sql.Date](Format.`date`))
  implicit val localDateS: Predef[java.time.LocalDate] = Predef(
    `string`[java.time.LocalDate](Format.`date`)
  )
  implicit val jsqlTimeS: Predef[java.sql.Time]        = Predef(`string`[java.sql.Time](Format.`time`))
  implicit val localTimeS: Predef[java.time.LocalTime] = Predef(
    `string`[java.time.LocalTime](Format.`time`)
  )
  implicit val jDurationS: Predef[java.time.Duration] = Predef(
    `string`[java.time.Duration](Format.`duration`)
  )
  implicit val durationS: Predef[Duration] = Predef(`string`[Duration](Format.`duration`))

  // string-likes (#475, docs/design/475-string-like.md): a standard format where one exists, a pattern where the text has a fixed shape, else a plain string
  private val currencyPattern = "^[A-Z]{3}$"                     // ISO 4217 alphabetic code
  private val charsetPattern  = "^[A-Za-z0-9][A-Za-z0-9+:_.-]*$" // the legal charset name rule of java.nio.charset.Charset

  implicit val localeS: Predef[java.util.Locale]     = Predef(`string`[java.util.Locale]) // `en_US` (toString) or `en-US` (toLanguageTag): no pattern fits both
  implicit val currencyS: Predef[java.util.Currency] = Predef(
    `string`[java.util.Currency].withValidation(`pattern` := currencyPattern)
  )
  implicit val patternS: Predef[java.util.regex.Pattern] = Predef(`string`[java.util.regex.Pattern](Format.`regex`))
  implicit val regexS: Predef[scala.util.matching.Regex] = Predef(`string`[scala.util.matching.Regex](Format.`regex`))
  // either family member; `oneOf [ipv4, ipv6]` would reject every value where formats are annotations (2019-09+)
  implicit val inetAddressS: Predef[java.net.InetAddress]   = Predef(`string`[java.net.InetAddress])
  implicit val inet4AddressS: Predef[java.net.Inet4Address] = Predef(`string`[java.net.Inet4Address](Format.`ipv4`))
  implicit val inet6AddressS: Predef[java.net.Inet6Address] = Predef(`string`[java.net.Inet6Address](Format.`ipv6`))
  implicit val pathS: Predef[java.nio.file.Path]            = Predef(`string`[java.nio.file.Path])
  implicit val fileS: Predef[java.io.File]                  = Predef(`string`[java.io.File])
  implicit val charsetS: Predef[java.nio.charset.Charset]   = Predef(
    `string`[java.nio.charset.Charset].withValidation(`pattern` := charsetPattern)
  )

  implicit def arrayS[T](implicit p: Predef[T]): Predef[Array[T]] = Predef(
    `array`[T, Array](p.schema, unique = false)
  )
  implicit def setS[T](implicit p: Predef[T]): Predef[Set[T]] = Predef(
    `array`[T, Set](p.schema, unique = true)
  )
  implicit def listS[T](implicit p: Predef[T]): Predef[List[T]] = Predef(
    `array`[T, List](p.schema, unique = false)
  )
  implicit def vectorS[T](implicit p: Predef[T]): Predef[Vector[T]] = Predef(
    `array`[T, Vector](p.schema, unique = false)
  )
  implicit def dictS[K, V](implicit p: Predef[V], keyP: KeyPattern[K]): Predef[Map[K, V]] = Predef {
    val schema = `dictionary`[K, V, Map](p.schema)
    if (keyP == `dictionary`.KeyPattern.StringKeyPattern) schema
    else {
      schema.withValidation[Map[K, V], Map[_, _]](`patternProperties` := keyP.pattern)
    }
  }
}
