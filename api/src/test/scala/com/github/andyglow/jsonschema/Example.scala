package com.github.andyglow.jsonschema

import java.time.LocalDateTime

import com.github.andyglow.json.JsonFormatter
import json.Json

object Example {
  import ExampleJsonSchema._

  def main(args: Array[String]): Unit = {
    println(JsonFormatter.format(AsValue.schema(personJsonType)))
  }
}

object ExampleMsg {

  sealed trait Gender

  object Gender {

    case object Male extends Gender

    case object Female extends Gender
  }

  case class Company(name: String)

  case class Car(name: String, manufacturer: Company)

  case class Person(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    gender: Gender,
    birthDay: LocalDateTime,
    company: Company,
    cars: Seq[Car])

}

object ExampleJsonSchema {
  import ExampleMsg._
  import json.Schema._

  implicit val genderJsonType: json.Schema[Gender] = Json.schema[Gender]

  implicit val companyJsonType: json.Schema[Company] = {
    val s = Json.schema[Company].asInstanceOf[`object`[Company]]


    s.copy(fields = s.fields map { x => x.copy(tpe = x.tpe.asInstanceOf[`string`[String]].copy(pattern = Some("\\d+"))) })
  }

  implicit val carJsonType: json.Schema[Car] = Json.schema[Car]

  implicit val personJsonType: json.Schema[Person] = Json.schema[Person]
}