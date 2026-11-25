package json.schema.validation

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Implicit not found: validation.Magnet[${F}, ${T}]. Some of validations doesn't match schema type"
)
trait Magnet[F, T] {

  def append(seq: collection.Seq[Def[_, _]], item: Def[T, _]): collection.Seq[Def[_, _]] =
    seq :+ item
}

trait LowPriorityMagnet {

  // Low priority: on Scala 3 the `TT >: T` type parameter of `withValidation` is still open when the magnet is
  // searched, so `identityMagnet` would otherwise be ambiguous with the more specific magnets below (e.g. `mapMagnet`).
  implicit def identityMagnet[X]: Magnet[X, X] = Magnet.mk[X, X]
}

// Collection magnets, by priority: the exact collection (in `object Magnet`) > `Seq` > `Iterable`. On Scala 3 `TT >: T` is open, so
// for a `List[X]` the magnets of its supertypes match too (`Iterable[X]` is a valid `TT`), and the levels keep that from being
// ambiguous ("listMagnet vs iterableMagnet"). Scala 2 fixes `TT = T` before the search, so there only one of them ever matches.
trait IterableMagnet extends LowPriorityMagnet {
  implicit def iterableMagnet[X]: Magnet[Iterable[X], Iterable[_]] = Magnet.mk[Iterable[X], Iterable[_]]
}

trait SeqMagnet extends IterableMagnet {
  // Scala 2 had no magnet for `Seq` itself (`Json.schema[Seq[X]].withValidation(...)` needed explicit type arguments)
  implicit def seqMagnet[X]: Magnet[Seq[X], Iterable[_]] = Magnet.mk[Seq[X], Iterable[_]]
}

object Magnet extends SeqMagnet {
  def mk[A, B]: Magnet[A, B] = new Magnet[A, B] {}

  implicit def numericMagnet[X: Numeric]: Magnet[X, Number] = mk[X, Number]

  implicit def intMapMagnet[X]: Magnet[Map[Int, X], Map[Int, _]]          = mk[Map[Int, X], Map[Int, _]]
  implicit def stringMapMagnet[X]: Magnet[Map[String, X], Map[String, _]] =
    mk[Map[String, X], Map[String, _]]
  implicit def mapMagnet[K, V]: Magnet[Map[K, V], Map[_, _]] = mk[Map[K, V], Map[_, _]]

  implicit def arrayMagnet[X]: Magnet[Array[X], Iterable[_]]   = mk[Array[X], Iterable[_]]
  implicit def listMagnet[X]: Magnet[List[X], Iterable[_]]     = mk[List[X], Iterable[_]]
  implicit def vectorMagnet[X]: Magnet[Vector[X], Iterable[_]] = mk[Vector[X], Iterable[_]]
  implicit def setMagnet[X]: Magnet[Set[X], Iterable[_]]       = mk[Set[X], Iterable[_]]

  // Java's boxed numbers and java.math have no `Numeric`. One magnet per type: a generic `X <: java.lang.Number` would be
  // ambiguous with `numericMagnet` for `scala.BigDecimal` / `BigInt`, which extend `java.lang.Number`.
  implicit def jIntegerMagnet: Magnet[java.lang.Integer, Number]       = mk[java.lang.Integer, Number]
  implicit def jLongMagnet: Magnet[java.lang.Long, Number]             = mk[java.lang.Long, Number]
  implicit def jShortMagnet: Magnet[java.lang.Short, Number]           = mk[java.lang.Short, Number]
  implicit def jByteMagnet: Magnet[java.lang.Byte, Number]             = mk[java.lang.Byte, Number]
  implicit def jFloatMagnet: Magnet[java.lang.Float, Number]           = mk[java.lang.Float, Number]
  implicit def jDoubleMagnet: Magnet[java.lang.Double, Number]         = mk[java.lang.Double, Number]
  implicit def jBigDecimalMagnet: Magnet[java.math.BigDecimal, Number] = mk[java.math.BigDecimal, Number]
  implicit def jBigIntegerMagnet: Magnet[java.math.BigInteger, Number] = mk[java.math.BigInteger, Number]

  implicit def chrMagnet: Magnet[Char, String]            = mk[Char, String]
  implicit def characterMagnet: Magnet[Character, String] = mk[Character, String]
  implicit def uuidMagnet: Magnet[java.util.UUID, String] = mk[java.util.UUID, String]

  // java.time types whose Predef is a string with a pattern (#474)
  implicit def periodMagnet: Magnet[java.time.Period, String]         = mk[java.time.Period, String]
  implicit def yearMagnet: Magnet[java.time.Year, String]             = mk[java.time.Year, String]
  implicit def yearMonthMagnet: Magnet[java.time.YearMonth, String]   = mk[java.time.YearMonth, String]
  implicit def monthDayMagnet: Magnet[java.time.MonthDay, String]     = mk[java.time.MonthDay, String]
  implicit def zoneOffsetMagnet: Magnet[java.time.ZoneOffset, String] = mk[java.time.ZoneOffset, String]

  // string-likes (#475): only where a core Predef attaches a validation
  implicit def currencyMagnet: Magnet[java.util.Currency, String]      = mk[java.util.Currency, String]
  implicit def charsetMagnet: Magnet[java.nio.charset.Charset, String] = mk[java.nio.charset.Charset, String]
}
