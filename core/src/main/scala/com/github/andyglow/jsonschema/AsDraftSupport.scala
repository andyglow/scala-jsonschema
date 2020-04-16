package com.github.andyglow.jsonschema

import json._
import json.Schema._

import com.github.andyglow.json.Value._


trait AsDraftSupport {

  def apply(x: json.Schema[_]): obj = apply(x, includeType = true)

  def apply(x: json.Schema[_], includeType: Boolean): obj = {
    val (validations, pp) = inferValidations(x)
    val specifics         = inferSpecifics.lift((pp, x)) getOrElse obj()
    val base              = x match {
      case _: `ref`[_] | _: `oneof`[_] => obj()
      case _ if includeType            => obj("type" -> x.jsonType)
      case _                           => obj()
    }

    base ++ validations ++ specifics
  }

  def mkStr(pp: Option[ValidationDef[_, _]], x: `string`[_]): obj = obj(
    ("format", x.format map { _.productPrefix }),
    ("pattern", x.pattern))

  def mkObj(pp: Option[ValidationDef[_, _]], x: `object`[_]): obj = {
    val props = x.fields.map { field =>
      val d = field.default map { d => obj("default" -> d) } getOrElse obj()

      field.name -> ( d ++ apply(field.tpe))
    }.toMap

    val required = x.fields collect {
      case field if field.required => str(field.name)
    }

    obj(
      ("additionalProperties", false),
      ("properties", obj(props)),
      ("required"  , arr(required.toSeq)))
  }

  def mkStrMap(pp: Option[ValidationDef[_, _]], comp: Schema[_]): obj = {
    val pattern = pp map { _.json.asInstanceOf[str].value } getOrElse "^.*$"
    obj("patternProperties" -> obj(
      pattern -> apply(comp)))
  }

  def mkIntMap(pp: Option[ValidationDef[_, _]], comp: Schema[_]): obj = {
    obj("patternProperties" -> obj(
      "^[0-9]*$" -> apply(comp)))
  }

  def mkArr(pp: Option[ValidationDef[_, _]], comp: Schema[_]): obj = {
    obj("items" -> apply(comp))
  }

  def mkSet(pp: Option[ValidationDef[_, _]], comp: Schema[_]): obj = {
    obj(
      "items" -> apply(comp),
      "uniqueItems" -> true)
  }

  def mkEnum(pp: Option[ValidationDef[_, _]], x: `enum`[_]): obj = {
    obj(
      "type" -> "string",
      "enum" -> arr(x.values.toSeq))
  }

  def mkOneOf(pp: Option[ValidationDef[_, _]], x: `oneof`[_]): obj = {
    val subTypesSeq = x.subTypes.toSeq
    val tpe = subTypesSeq.head.jsonType
    val sameType = subTypesSeq.tail.foldLeft(true) { case (agg, t) => agg && (t.jsonType == tpe) }
    if (sameType) {
      obj(
        "type"  -> `str`(tpe),
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, includeType = false),
          subTypesSeq.tail.map(t => apply(t, includeType = false)): _*))
    } else
      obj(
        "oneOf" -> `arr`(
          apply(subTypesSeq.head),
          subTypesSeq.tail.map(apply): _*))
  }

  def mkAllOf(pp: Option[ValidationDef[_, _]], x: `allof`[_]): obj = {
    val subTypesSeq = x.subTypes.toSeq
    val tpe = subTypesSeq.head.jsonType
    val sameType = subTypesSeq.tail.foldLeft(true) { case (agg, t) => agg && (t.jsonType == tpe) }
    if (sameType) {
      obj(
        "type"  -> `str`(tpe),
        "allOf" -> `arr`(
          apply(subTypesSeq.head, includeType = false),
          subTypesSeq.tail.map(t => apply(t, includeType = false)): _*))
    } else
      obj(
        "allOf" -> `arr`(
          apply(subTypesSeq.head),
          subTypesSeq.tail.map(apply): _*))
  }


  def mkNot(pp: Option[ValidationDef[_, _]], x: `not`[_]): obj = {
    obj("not" -> apply(x.tpe, includeType = false))
  }

  def mkRef(pp: Option[ValidationDef[_, _]], x: `ref`[_]): obj = {
    val ref = x.tpe.refName getOrElse x.sig
    obj(f"$$ref" -> s"#/definitions/$ref")
  }

  val inferSpecifics: PartialFunction[(Option[ValidationDef[_, _]], json.Schema[_]), obj] = {
    case (pp, x: `string`[_])        => mkStr(pp, x)
    case (pp, x: `object`[_])        => mkObj(pp, x)
    case (pp, `string-map`(comp))    => mkStrMap(pp, comp)
    case (pp, `int-map`(comp))       => mkIntMap(pp, comp)
    case (pp, `array`(comp))         => mkArr(pp, comp)
    case (pp, `set`(comp))           => mkSet(pp, comp)
    case (pp, x: `enum`[_])          => mkEnum(pp, x)
    case (pp, x: `oneof`[_])         => mkOneOf(pp, x)
    case (pp, x: `allof`[_])         => mkAllOf(pp, x)
    case (pp, x: `not`[_])           => mkNot(pp, x)
    case (pp, x: `ref`[_])           => mkRef(pp, x)
  }

  def inferValidations(x: json.Schema[_]): (obj, Option[ValidationDef[_, _]]) = {
    import Validation._

    val pp = x.validations.find(_.validation == `patternProperties`)
    val validations = obj {
      x.validations.collect {
        case d if d.validation != `patternProperties` =>
          d.validation.name -> d.json
      }.toMap
    }

    (validations, pp)
  }

  def inferDefinition(x: `ref`[_]): (String, obj) = {
    val ref = x.tpe.refName getOrElse x.sig
    ref -> apply(x.tpe)
  }

  def inferDefinitions(x: Schema[_]): obj = {
    def references(tpe: json.Schema[_]): Seq[`ref`[_]] = tpe match {
      case x: `ref`[_]      => references(x.tpe) :+ x
      case `array`(ref)     => references(ref)
      case `object`(fields) => fields.toSeq map { _.tpe } flatMap references
      case _                => Seq.empty
    }

    obj {
      references(x).map(inferDefinition).toMap
    }
  }
}
