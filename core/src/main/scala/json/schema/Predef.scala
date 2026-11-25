package json.schema

import java.net.{URI, URL}
import java.util.UUID

import com.github.andyglow.json.Value
import json._
import json.Schema._
import json.Schema.`dictionary`.KeyPattern
import json.Schema.`string`.Format
import json.schema.validation.Instance._

import scala.concurrent.duration.{Duration, FiniteDuration}

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

  /** The bridge from `import json.Json.auto._` (issue #542, as circe's `importedEncoder`): auto gives an [[Exported]] `Predef`, which never competes with a real `Predef` and is reached only through
    * this lowest-priority instance. So every `Predef` or `Schema` the user or a library defines wins over auto.
    */
  implicit def importedPredef[T](implicit exported: Exported[Predef[T]]): Predef[T] = exported.instance
}
object Predef extends LowPriorityPredefs {
  implicit val strS: Predef[String] = Predef(`string`[String])
  // exactly one character: the lengths say it, no pattern (#463). JSON Schema counts code points, so a non-BMP character, which no Char holds, is accepted too
  implicit val chrS: Predef[Char] = Predef(
    `string`[Char].withValidation(`minLength` := 1, `maxLength` := 1)
  )
  implicit val jchrS: Predef[Character] = Predef(
    `string`[Character].withValidation(`minLength` := 1, `maxLength` := 1)
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

  // Java's boxed types and java.math: the Scala type's own node, so they always render the same (the type parameter is
  // phantom for these nodes; `BoxedBigMathSpec` checks they stay equal)
  implicit val jIntegerS: Predef[java.lang.Integer]       = Predef(intS.schema.asInstanceOf[Schema[java.lang.Integer]])
  implicit val jLongS: Predef[java.lang.Long]             = Predef(longS.schema.asInstanceOf[Schema[java.lang.Long]])
  implicit val jShortS: Predef[java.lang.Short]           = Predef(shortS.schema.asInstanceOf[Schema[java.lang.Short]])
  implicit val jByteS: Predef[java.lang.Byte]             = Predef(byteS.schema.asInstanceOf[Schema[java.lang.Byte]])
  implicit val jDoubleS: Predef[java.lang.Double]         = Predef(doubleS.schema.asInstanceOf[Schema[java.lang.Double]])
  implicit val jFloatS: Predef[java.lang.Float]           = Predef(floatS.schema.asInstanceOf[Schema[java.lang.Float]])
  implicit val jBooleanS: Predef[java.lang.Boolean]       = Predef(boolS.schema.asInstanceOf[Schema[java.lang.Boolean]])
  implicit val jBigDecimalS: Predef[java.math.BigDecimal] = Predef(bigDecimalS.schema.asInstanceOf[Schema[java.math.BigDecimal]])
  implicit val jBigIntegerS: Predef[java.math.BigInteger] = Predef(bigIntS.schema.asInstanceOf[Schema[java.math.BigInteger]])

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

  // #474. A format where it matches the text the codecs write; else a pattern of that written form (`toString` and the ISO formatters), see docs/design/474-java-time.md
  implicit val offsetDateTimeS: Predef[java.time.OffsetDateTime] = Predef(
    `string`[java.time.OffsetDateTime](Format.`date-time`)
  )
  implicit val offsetTimeS: Predef[java.time.OffsetTime] = Predef(`string`[java.time.OffsetTime](Format.`time`))
  // the same as `Duration` (a Predef is invariant, so `durationS` doesn't apply to the subtype)
  implicit val finiteDurationS: Predef[FiniteDuration] = Predef(`string`[FiniteDuration](Format.`duration`))
  implicit val periodS: Predef[java.time.Period]       = Predef(
    `string`[java.time.Period].withValidation(`pattern` := "^P(?:-?[0-9]+Y(?:-?[0-9]+M)?(?:-?[0-9]+D)?|-?[0-9]+M(?:-?[0-9]+D)?|-?[0-9]+D)$")
  )
  implicit val yearS: Predef[java.time.Year]           = Predef(`string`[java.time.Year].withValidation(`pattern` := "^[-+]?[0-9]+$"))
  implicit val yearMonthS: Predef[java.time.YearMonth] = Predef(
    `string`[java.time.YearMonth].withValidation(`pattern` := "^[-+]?[0-9]{4,}-(?:0[1-9]|1[0-2])$")
  )
  implicit val monthDayS: Predef[java.time.MonthDay] = Predef(
    `string`[java.time.MonthDay].withValidation(`pattern` := "^--(?:0[1-9]|1[0-2])-(?:0[1-9]|[12][0-9]|3[01])$")
  )
  implicit val zoneOffsetS: Predef[java.time.ZoneOffset] = Predef(
    `string`[java.time.ZoneOffset].withValidation(`pattern` := "^(?:Z|[-+][0-9]{2}:[0-9]{2}(?::[0-9]{2})?)$")
  )
  // a region id is any `Area/City` name, so no pattern; as a Predef it also keeps the macros from deriving the abstract class as a family
  implicit val zoneIdS: Predef[java.time.ZoneId] = Predef(`string`[java.time.ZoneId])

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

  /** `Array[Byte]` as a base64 string (what Jackson writes): `{"type": "string", "contentEncoding": "base64", "pattern": ...}` (standard alphabet, padded, no line feeds). Not implicit: the default,
    * from [[arrayS]], is an array of numbers (what circe, play-json, upickle, spray-json and json4s write). Opt in with `implicit val bytes: Predef[Array[Byte]] = Predef.bytesAsBase64`.
    */
  val bytesAsBase64: Predef[Array[Byte]] = Predef(
    `string`[Array[Byte]].withValidation(
      `contentEncoding` := "base64",
      `pattern`         := "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$"
    )(validation.Magnet.mk[Array[Byte], String])
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

  // our own JSON AST: any value, and each kind of value (the JSON libraries' ASTs: `AsX.predef` in each bridge)
  implicit val jsonValueS: Predef[Value]            = Predef(`any`[Value]())
  implicit val jsonObjS: Predef[Value.obj]          = Predef(`object`.Free[Value.obj]())
  implicit val jsonArrS: Predef[Value.arr]          = Predef(`array`[Value, Seq](`any`[Value]()).asInstanceOf[Schema[Value.arr]])
  implicit val jsonStrS: Predef[Value.str]          = Predef(`string`[Value.str])
  implicit val jsonNumS: Predef[Value.num]          = Predef(`number`[BigDecimal].asInstanceOf[Schema[Value.num]])
  implicit val jsonBoolS: Predef[Value.bool]        = Predef(`boolean`.asInstanceOf[Schema[Value.bool]])
  implicit val jsonNullS: Predef[Value.`null`.type] = Predef(`null`.asInstanceOf[Schema[Value.`null`.type]])
}
