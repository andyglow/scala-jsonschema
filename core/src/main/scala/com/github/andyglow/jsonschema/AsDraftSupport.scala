package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.schema.{ validation => V }

import com.github.andyglow.json.Value._


trait AsDraftSupport {

  def apply(x: json.Schema[_]): obj = apply(x, includeType = true, isRoot = true)

  def apply(x: json.Schema[_], includeType: Boolean, isRoot: Boolean): obj = {
    val (validations, pp) = inferValidations(x)
    val specifics         = inferSpecifics.lift((pp, x, isRoot)) getOrElse obj()
    val base              = x match {
      case _: `ref`[_] | _: `allof`[_] | _: `oneof`[_] | _: `lazy-ref`[_] => obj()
      case _ if includeType                                               => obj("type" -> x.jsonType)
      case _                                                              => obj()
    }

    base ++ validations ++ specifics
  }

  def mkStr(pp: Option[V.Def[_, _]], x: `string`[_]): obj = obj(
    ("format", x.format map { _.productPrefix }))

  def mkObj(pp: Option[V.Def[_, _]], x: `object`[_]): obj = {
    val props = x.fields.map { field =>
      val default = field.default map { d => obj("default" -> d) } getOrElse obj()
      val description = field.description map { d => obj("description" -> d) } getOrElse obj()

      field.name -> ( default ++ apply(field.tpe, includeType = true, isRoot = false) ++ description )
    }.toMap

    val required = x.fields collect {
      case field if field.required => str(field.name)
    }

    val canHaveAdditionalProperties = x.isInstanceOf[`object`.Free]

    obj(
      ("additionalProperties", canHaveAdditionalProperties),
      ("properties", if (props.isEmpty) None else Some(obj(props))),
      ("required"  , if (required.isEmpty) None else Some(arr(required.toSeq))))
  }

  def mkDict(pp: Option[V.Def[_, _]], comp: Schema[_]): obj = {
    val pattern = pp map { _.json.asInstanceOf[str].value } getOrElse "^.*$"
    obj("patternProperties" -> obj(
      pattern -> apply(comp, includeType = true, isRoot = false)))
  }

  def mkArr(pp: Option[V.Def[_, _]], comp: Schema[_], unique: Boolean): obj = {
    obj(
      "items" -> apply(comp, includeType = true, isRoot = false),
      "uniqueItems" -> (if (unique) Some(true) else None))
  }

  def mkEnum(pp: Option[V.Def[_, _]], x: `enum`[_]): obj = {
    obj(
      "type" -> "string",
      "enum" -> arr(x.values.toSeq))
  }

  def mkOneOf(pp: Option[V.Def[_, _]], x: `oneof`[_], isRoot: Boolean): obj = {
    val subTypesSeq = x.subTypes.toSeq
    val tpe = subTypesSeq.head.jsonType
    val sameType = subTypesSeq.tail.foldLeft(true) { case (agg, t) => agg && (t.jsonType == tpe) }
    if (sameType) {
      obj(
        "type"  -> `str`(tpe),
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, includeType = false, isRoot = false),
          subTypesSeq.tail.map(t => apply(t, includeType = false, isRoot = false)): _*))
    } else
      obj(
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, includeType = true, isRoot = false),
          subTypesSeq.tail.map(apply(_, includeType = true, isRoot = false)): _*))
  }

  def mkAllOf(pp: Option[V.Def[_, _]], x: `allof`[_], isRoot: Boolean): obj = {
    val subTypesSeq = x.subTypes.toSeq
    val tpe = subTypesSeq.head.jsonType
    val sameType = subTypesSeq.tail.foldLeft(true) { case (agg, t) => agg && (t.jsonType == tpe) }
    if (sameType) {
      obj(
        "type"  -> (if (!isRoot) Some(`str`(tpe)) else None),
        "allOf" -> `arr`(
          apply(subTypesSeq.head, includeType = false, isRoot = false),
          subTypesSeq.tail.map(t => apply(t, includeType = false, isRoot = false)): _*))
    } else
      obj(
        "allOf" -> `arr`(
          apply(subTypesSeq.head, includeType = true, isRoot = false),
          subTypesSeq.tail.map(apply(_, includeType = true, isRoot = false)): _*))
  }


  def mkNot(pp: Option[V.Def[_, _]], x: `not`[_]): obj = {
    obj("not" -> apply(x.tpe, includeType = false, isRoot = false))
  }

  def mkRef(pp: Option[V.Def[_, _]], x: `ref`[_]): obj = {
    val ref = x.tpe.refName getOrElse x.sig
    obj(f"$$ref" -> buildRef(ref))
  }

  def mkLazyRef(pp: Option[V.Def[_, _]], x: `lazy-ref`[_]): obj = {
    val ref = x.sig
    obj(f"$$ref" -> buildRef(ref))
  }

  def buildRef(ref: String): String = s"#/definitions/$ref"

  def mkValueClass(pp: Option[V.Def[_, _]], x: `value-class`[_, _]): obj = {
    inferSpecifics.lift((pp, x.tpe, false)) getOrElse obj()
  }

  val inferSpecifics: PartialFunction[(Option[V.Def[_, _]], json.Schema[_], Boolean), obj] = {
    case (pp, x: `string`[_], _)           => mkStr(pp, x)
    case (pp, x: `object`[_], _)           => mkObj(pp, x)
    case (pp, `dictionary`(comp), _)       => mkDict(pp, comp)
    case (pp, `array`(comp, unique), _)    => mkArr(pp, comp, unique)
    case (pp, x: `enum`[_], _)             => mkEnum(pp, x)
    case (pp, x: `oneof`[_], isRoot)       => mkOneOf(pp, x, isRoot)
    case (pp, x: `allof`[_], isRoot)       => mkAllOf(pp, x, isRoot)
    case (pp, x: `not`[_], _)              => mkNot(pp, x)
    case (pp, x: `ref`[_], _)              => mkRef(pp, x)
    case (pp, x: `lazy-ref`[_], _)         => mkLazyRef(pp, x)
    case (pp, x: `value-class`[_, _], _)   => mkValueClass(pp, x)
  }

  def inferValidations(x: json.Schema[_]): (obj, Option[V.Def[_, _]]) = {
    import V.Instance._

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
    ref -> apply(x.tpe, includeType = true, isRoot = false)
  }

  def inferDefinitions(x: Schema[_]): obj = {
    def references(tpe: json.Schema[_]): Seq[`ref`[_]] = tpe match {
      case x: `ref`[_]      => references(x.tpe) :+ x
      case x: `allof`[_]    => x.subTypes.toSeq flatMap references
      case x: `oneof`[_]    => x.subTypes.toSeq flatMap references
      case `array`(ref, _)  => references(ref)
      case `object`(fields) => fields.toSeq map { _.tpe } flatMap references
      case _                => Seq.empty
    }

    obj {
      references(x).map(inferDefinition).toMap
    }
  }
}
