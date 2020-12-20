package com.github.andyglow.json

import json.Schema
import json.Schema._


sealed trait ValueSchema[T] {
  type S
  def schema: json.Schema[S]
}

object ValueSchema {

  implicit val schemaForStr: ValueSchema[Value.str] = new ValueSchema[Value.str] {
    override type S = String
    override def schema: Schema[S] = `string`
  }

  implicit val schemaForNum: ValueSchema[Value.num] = new ValueSchema[Value.num] {
    override type S = Long
    override def schema: Schema[S] = `number`[S]
  }

  implicit val schemaForBool: ValueSchema[Value.bool] = new ValueSchema[Value.bool] {
    override type S = Boolean
    override def schema: Schema[S] = `boolean`
  }

  implicit val schemaForString: ValueSchema[String] = new ValueSchema[String] {
    override type S = String
    override def schema: Schema[S] = `string`
  }

  implicit val schemaForInteger: ValueSchema[Int] = new ValueSchema[Int] {
    override type S = Int
    override def schema: Schema[S] = `integer`
  }

  implicit val schemaForLong: ValueSchema[Long] = new ValueSchema[Long] {
    override type S = Int
    override def schema: Schema[S] = `integer`
  }

  implicit val schemaForByte: ValueSchema[Byte] = new ValueSchema[Byte] {
    override type S = Int
    override def schema: Schema[S] = `integer`
  }

  implicit val schemaForShort: ValueSchema[Short] = new ValueSchema[Short] {
    override type S = Int
    override def schema: Schema[S] = `integer`
  }

  implicit def schemaForNumeric[T: Numeric]: ValueSchema[T] = new ValueSchema[T] {
    override type S = T
    override def schema: Schema[S] = `number`[S]
  }

  implicit val schemaForBoolean: ValueSchema[Boolean] = new ValueSchema[Boolean] {
    override type S = Boolean
    override def schema: Schema[S] = `boolean`
  }
}