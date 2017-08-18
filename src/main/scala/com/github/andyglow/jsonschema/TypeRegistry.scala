package com.github.andyglow.jsonschema

import json.Schema

import scala.collection.concurrent.TrieMap

trait TypeRegistry {

  def resolve[T](sig: TypeSignature[T], default: => Schema): Schema

  def get[T](sig: TypeSignature[T]): Option[Schema]
}

trait LowPriorityTypeRegistries {

  implicit object empty extends TypeRegistry {

    def resolve[T](sig: TypeSignature[T], default: => Schema): Schema = default

    def get[T](sig: TypeSignature[T]): Option[Schema] = None
  }
}

object TypeRegistry extends LowPriorityTypeRegistries {

  class Default extends TypeRegistry {

    private val schemas = TrieMap.empty[TypeSignature[_], Schema]

    def register[T](sig: TypeSignature[T], schema: Schema): Default = {
      schemas.put(sig, schema)
      this
    }

    def get[T](sig: TypeSignature[T]): Option[Schema] = schemas get sig

    def resolve[T](key: TypeSignature[T], default: => Schema): Schema = {
      schemas.getOrElseUpdate(key, default) match {
        case _: Schema.Def    => Schema.Ref(key)
        case x: Schema.Ref[_] => x
      }
    }

    def asMap: Map[TypeSignature[_], Schema] = schemas.toMap
  }
}