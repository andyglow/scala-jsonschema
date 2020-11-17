package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema._
import json.Schema.`dictionary`.KeyPattern
import json.schema.validation.Magnet
import json.schema.validation.Instance._
import json.schema.Predef

trait LowPriorityCatsSupport extends ScalaVersionSpecificLowPriorityCatsSupport {
  import Magnet.mk

  // bounds
  implicit def chainVB[X]: Magnet[Chain[X], Iterable[_]] = mk[Chain[X], Iterable[_]]
  implicit def nelVB[X]: Magnet[NonEmptyList[X], Iterable[_]] = mk[NonEmptyList[X], Iterable[_]]
  implicit def nevVB[X]: Magnet[NonEmptyVector[X], Iterable[_]] = mk[NonEmptyVector[X], Iterable[_]]
  implicit def nesVB[X]: Magnet[NonEmptySet[X], Iterable[_]] = mk[NonEmptySet[X], Iterable[_]]
  implicit def necVB[X]: Magnet[NonEmptyChain[X], Iterable[_]] = mk[NonEmptyChain[X], Iterable[_]]
  implicit def nemStrVB[K, V]: Magnet[NonEmptyMap[K, V], Map[_, _]] = mk[NonEmptyMap[K, V], Map[_, _]]
  implicit def oneAndVB[F[_], X]: Magnet[OneAnd[F, X], Iterable[_]] = mk[OneAnd[F, X], Iterable[_]]

  protected def mkNEx[T, C[_]](schema: Schema[T])(implicit b: Magnet[C[T], Iterable[_]]) = Predef(`array`[T, C](schema).withValidation(`minItems` := 1))
  protected def mkNESM[K, V](vSchema: Schema[V], keyP: KeyPattern[K])(implicit b: Magnet[NonEmptyMap[K, V], Map[_, _]]) = Predef {
    val schema = `dictionary`[K, V, NonEmptyMap](vSchema).withValidation(`minProperties` := 1)
    if (keyP == `dictionary`.KeyPattern.StringKeyPattern) schema else {
      schema.withValidation(`patternProperties` := keyP.pattern)
    }
  }

  implicit def chainSchemaFromPredef[T](implicit p: Predef[T]): Predef[Chain[T]] = mkNEx[T, Chain](p.schema)

  implicit def oneAndSchemaFromPredef[F[_], T](implicit p: Predef[T], evidence$1: Predef[F[T]]): Predef[OneAnd[F, T]] = Predef(`array`[T, ({type Z[X] = OneAnd[F, X]})#Z](p.schema).withValidation(`minItems` := 1))

  implicit def nelSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyList[T]] = mkNEx[T, NonEmptyList](p.schema)

  implicit def nevSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyVector[T]] = mkNEx[T, NonEmptyVector](p.schema)

  implicit def nesSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptySet[T]] = mkNEx[T, NonEmptySet](p.schema)

  implicit def necSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyChain[T]] = mkNEx[T, NonEmptyChain](p.schema)

  implicit def nemStrSchemaFromPredef[K, V](implicit p: Predef[V], keyP: KeyPattern[K]): Predef[NonEmptyMap[K, V]] = mkNESM(p.schema, keyP)
}

object CatsSupport extends LowPriorityCatsSupport with ScalaVersionSpecificCatsSupport {

  implicit def chainSchema[T](implicit ss: Schema[T]): Predef[Chain[T]] = mkNEx[T, Chain](ss)

  implicit def oneAndSchema[F[_], T](implicit ss: Schema[T], _1: Schema[F[T]]): Predef[OneAnd[F, T]] = Predef(`array`[T, ({type Z[X] = OneAnd[F, X]})#Z](ss).withValidation(`minItems` := 1))

  implicit def nelSchema[T](implicit ss: Schema[T]): Predef[NonEmptyList[T]] = mkNEx[T, NonEmptyList](ss)

  implicit def nevSchema[T](implicit ss: Schema[T]): Predef[NonEmptyVector[T]] = mkNEx[T, NonEmptyVector](ss)

  implicit def nesSchema[T](implicit ss: Schema[T]): Predef[NonEmptySet[T]] = mkNEx[T, NonEmptySet](ss)

  implicit def necSchema[T](implicit ss: Schema[T]): Predef[NonEmptyChain[T]] = mkNEx[T, NonEmptyChain](ss)

  implicit def nemStrSchema[K, V](implicit ss: Schema[V], keyP: KeyPattern[K]): Predef[NonEmptyMap[K, V]] = mkNESM(ss, keyP)

}
