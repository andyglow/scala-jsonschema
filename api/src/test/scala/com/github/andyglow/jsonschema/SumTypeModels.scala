package com.github.andyglow.jsonschema

import json.schema.{description, title}


object SumTypeModels {

  sealed trait FooBarInsideCompanionWithAnnotations extends Any
  object FooBarInsideCompanionWithAnnotations {
    @title("t1")
    @description("d1")
    case class M1(val1: Double) extends FooBarInsideCompanionWithAnnotations

    @title("t2")
    @description("d2")
    case class M2(val2: Double) extends FooBarInsideCompanionWithAnnotations

    @title("t3")
    @description("d3")
    case class M3(val3: String) extends AnyVal with FooBarInsideCompanionWithAnnotations

    @title("t4")
    @description("d4")
    case object M4 extends FooBarInsideCompanionWithAnnotations

    @title("t5")
    @description("d5")
    case object M5 extends FooBarInsideCompanionWithAnnotations
  }
}