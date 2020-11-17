package json.schema.validation

import scala.annotation.implicitNotFound

@implicitNotFound("Implicit not found: validation.Magnet[${F}, ${T}]. Some of validations doesn't match schema type")
trait Magnet[F, T] {
  
  def append(
    seq: collection.Seq[Def[_, _]],
    item: Def[T, _]): collection.Seq[Def[_, _]] = seq :+ item
}

object Magnet {
  def mk[A, B]: Magnet[A, B] = new Magnet[A, B] {}

  implicit def identityMagnet[X]: Magnet[X, X] = mk[X, X]

  implicit def numericMagnet[X: Numeric]: Magnet[X, Number] = mk[X, Number]

  implicit def intMapMagnet[X]: Magnet[Map[Int, X], Map[Int, _]] = mk[Map[Int, X], Map[Int, _]]
  implicit def stringMapMagnet[X]: Magnet[Map[String, X], Map[String, _]] = mk[Map[String, X], Map[String, _]]
  implicit def mapMagnet[K, V]: Magnet[Map[K, V], Map[_, _]] = mk[Map[K, V], Map[_, _]]

  implicit def arrayMagnet[X]: Magnet[Array[X], Iterable[_]] = mk[Array[X], Iterable[_]]
  implicit def iterableMagnet[X]: Magnet[Iterable[X], Iterable[_]] = mk[Iterable[X], Iterable[_]]
  implicit def listMagnet[X]: Magnet[List[X], Iterable[_]] = mk[List[X], Iterable[_]]
  implicit def vectorMagnet[X]: Magnet[Vector[X], Iterable[_]] = mk[Vector[X], Iterable[_]]
  implicit def setMagnet[X]: Magnet[Set[X], Iterable[_]] = mk[Set[X], Iterable[_]]

  implicit def chrMagnet: Magnet[Char, String] = mk[Char, String]
  implicit def characterMagnet: Magnet[Character, String] = mk[Character, String]
  implicit def uuidMagnet: Magnet[java.util.UUID, String] = mk[java.util.UUID, String]
}
