package com.github.andyglow.jsonschema

import java.time.LocalDateTime
import com.github.andyglow.json.JsonFormatter
import json.Json
import json.schema._

object Example {
  import ExampleJsonSchema._

  def main(args: Array[String]): Unit = {
    println(JsonFormatter.format(AsValue.schema(personJsonType, Version.Draft04())))
  }
}

object ExampleMsg {

  sealed trait Gender

  object Gender {

    @typeHint[String] @title("The Male") case object Male extends Gender

    /** The Female
      */
    @typeHint[String] case object Female extends Gender
  }

  case class Company(name: String)

  case class Car(name: String, manufacturer: Company)

  /** Super Person
    *
    * @param firstName First
    * @param middleName Middle
    * @param lastName Last
    * @param gender Gender
    * @param birthDay Birth dat
    * @param company Company
    * @param cars Cars owned
    * @param active Is Active or not
    */
  case class Person(
    firstName: String,
    middleName: Option[String],
    lastName: String,
    gender: Gender,
    birthDay: LocalDateTime,
    company: Company,
    cars: Seq[Car],
    active: Boolean = true)

}

object ExampleJsonSchema {
  import ExampleMsg._

  implicit val jsonSchemaFlags: Flag with Flag.EnumsAsOneOf = null

  implicit val genderJsonType: json.Schema[Gender] = Json.schema[Gender]

  implicit val companyJsonType: json.Schema[Company] = Json.schema[Company]

  implicit val carJsonType: json.Schema[Car] = Json.schema[Car]

  val personJsonType: json.Schema[Person] = Json.schema[Person]
}