package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema._
import json.Validation._
import json.schema.Predef

trait LowPriorityCatsSupport extends ScalaVersionSpecificLowPriorityCatsSupport {
  import ValidationBound.mk

  // bounds
  implicit def chainVB[X]: ValidationBound[Chain[X], Iterable[_]] = mk[Chain[X], Iterable[_]]
  implicit def nelVB[X]: ValidationBound[NonEmptyList[X], Iterable[_]] = mk[NonEmptyList[X], Iterable[_]]
  implicit def nevVB[X]: ValidationBound[NonEmptyVector[X], Iterable[_]] = mk[NonEmptyVector[X], Iterable[_]]
  implicit def nesVB[X]: ValidationBound[NonEmptySet[X], Iterable[_]] = mk[NonEmptySet[X], Iterable[_]]
  implicit def necVB[X]: ValidationBound[NonEmptyChain[X], Iterable[_]] = mk[NonEmptyChain[X], Iterable[_]]
  implicit def nemStrVB[X]: ValidationBound[NonEmptyMap[String, X], Map[_, _]] = mk[NonEmptyMap[String, X], Map[_, _]]
  implicit def nemIntVB[X]: ValidationBound[NonEmptyMap[Int, X], Map[_, _]] = mk[NonEmptyMap[Int, X], Map[_, _]]
  implicit def oneAndVB[F[_], X]: ValidationBound[OneAnd[F, X], Iterable[_]] = mk[OneAnd[F, X], Iterable[_]]

  protected def mkNEx[T, C[_]](schema: Schema[T])(implicit b: ValidationBound[C[T], Iterable[_]]) = Predef(`array`[T, C](schema).withValidation(`minItems` := 1))
  protected def mkNESM[T](schema: Schema[T])(implicit b: ValidationBound[NonEmptyMap[String, T], Map[_, _]]) = Predef(`string-map`[T, NonEmptyMap](schema).withValidation(`minProperties` := 1))
  protected def mkNEIM[T](schema: Schema[T])(implicit b: ValidationBound[NonEmptyMap[Int, T], Map[_, _]]) = Predef(`int-map`[T, NonEmptyMap](schema).withValidation(`minProperties` := 1))

  implicit def chainSchemaFromPredef[T](implicit p: Predef[T]): Predef[Chain[T]] = mkNEx[T, Chain](p.schema)

  implicit def oneAndSchemaFromPredef[F[_], T](implicit p: Predef[T], evidence$1: Predef[F[T]]): Predef[OneAnd[F, T]] = Predef(`array`[T, ({type Z[X] = OneAnd[F, X]})#Z](p.schema).withValidation(`minItems` := 1))

  implicit def nelSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyList[T]] = mkNEx[T, NonEmptyList](p.schema)

  implicit def nevSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyVector[T]] = mkNEx[T, NonEmptyVector](p.schema)

  implicit def nesSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptySet[T]] = mkNEx[T, NonEmptySet](p.schema)

  implicit def necSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyChain[T]] = mkNEx[T, NonEmptyChain](p.schema)

  implicit def nemStrSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyMap[String, T]] = mkNESM(p.schema)

  implicit def nemIntSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyMap[Int, T]] = mkNEIM(p.schema)
}

object CatsSupport extends LowPriorityCatsSupport with ScalaVersionSpecificCatsSupport {

  implicit def chainSchema[T](implicit ss: Schema[T]): Predef[Chain[T]] = mkNEx[T, Chain](ss)

  implicit def oneAndSchema[F[_], T](implicit ss: Schema[T], _1: Schema[F[T]]): Predef[OneAnd[F, T]] = Predef(`array`[T, ({type Z[X] = OneAnd[F, X]})#Z](ss).withValidation(`minItems` := 1))

  implicit def nelSchema[T](implicit ss: Schema[T]): Predef[NonEmptyList[T]] = mkNEx[T, NonEmptyList](ss)

  implicit def nevSchema[T](implicit ss: Schema[T]): Predef[NonEmptyVector[T]] = mkNEx[T, NonEmptyVector](ss)

  implicit def nesSchema[T](implicit ss: Schema[T]): Predef[NonEmptySet[T]] = mkNEx[T, NonEmptySet](ss)

  implicit def necSchema[T](implicit ss: Schema[T]): Predef[NonEmptyChain[T]] = mkNEx[T, NonEmptyChain](ss)

  implicit def nemStrSchema[T](implicit ss: Schema[T]): Predef[NonEmptyMap[String, T]] = mkNESM(ss)

  implicit def nemIntSchema[T](implicit ss: Schema[T]): Predef[NonEmptyMap[Int, T]] = mkNEIM(ss)
}
