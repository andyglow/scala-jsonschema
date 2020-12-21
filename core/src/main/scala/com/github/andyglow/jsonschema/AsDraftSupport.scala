package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.schema.{validation => V}
import com.github.andyglow.json.Value._
import json.Schema.`object`.Field.RWMode


trait AsDraftSupport {

  type ParentSchema = Option[json.Schema[_]]

  def apply(x: json.Schema[_]): obj = apply(x, None, includeType = true, isRoot = true)

  def apply(x: json.Schema[_], parent: ParentSchema, includeType: Boolean, isRoot: Boolean): obj = {
    val (validations, pp) = inferValidations(x)
    val specifics         = inferSpecifics.lift((pp, x, parent, isRoot)) getOrElse obj()
    val base              = x match {
      case _: `def`[_] | _: `allof`[_] | _: `oneof`[_] | _: `ref`[_] => obj()
      case `value-class`(x) if includeType                           => obj("type" -> x.jsonType)
      case _ if includeType                                          => obj("type" -> x.jsonType)
      case _                                                         => obj()
    }

    base ++ validations ++ specifics
  }

  def mkStr(vv: Option[V.Def[_, _]], x: `string`[_], par: ParentSchema): obj = obj(
    ("format", x.format map { _.productPrefix }))

  def mkObj(vv: Option[V.Def[_, _]], x: `object`[_], par: ParentSchema): obj = {
    val discriminatorField = par match {
      case Some(`oneof`(_, f)) => f
      case _                   => None
    }
    val props = x.fields.map { field =>
      val default     = field.default map { d => obj("default" -> d) } getOrElse obj()
      val description = field.description map { d => obj("description" -> d) } getOrElse obj()
      val tpe         = apply(field.tpe, Some(x), includeType = true, isRoot = false)
      val rw          = field.rwMode match {
        case RWMode.ReadOnly  => obj("readOnly" -> true)
        case RWMode.WriteOnly => obj("writeOnly" -> true)
        case RWMode.ReadWrite => obj()
      }

      field.name -> (
        default ++
        tpe ++
        rw ++
        description)
    }.toMap ++ {
      // discrimination logic
      discriminatorField.flatMap { df =>
        for { dk <- x.discriminationKey } yield {
          df -> obj("enum" -> arr(dk))
        }
      }
    }

    val required = x.fields.collect {
      case field if field.required => str(field.name)
    } ++ discriminatorField.map(str)

    val canHaveAdditionalProperties = x.isInstanceOf[`object`.Free]

    obj(
      ("additionalProperties", canHaveAdditionalProperties),
      ("properties", if (props.isEmpty) None else Some(obj(props))),
      ("required"  , if (required.isEmpty) None else Some(arr(required.toSeq))))
  }

  def mkDict(vv: Option[V.Def[_, _]], comp: Schema[_], par: ParentSchema): obj = {
    val pattern = vv map { _.json.asInstanceOf[str].value } getOrElse "^.*$"
    obj("patternProperties" -> obj(
      pattern -> apply(comp, Some(comp), includeType = true, isRoot = false)))
  }

  def mkArr(vv: Option[V.Def[_, _]], comp: Schema[_], unique: Boolean, par: ParentSchema): obj = {
    obj(
      "items" -> apply(comp, par, includeType = true, isRoot = false),
      "uniqueItems" -> (if (unique) Some(true) else None))
  }

  def mkEnum(vv: Option[V.Def[_, _]], x: `enum`[_], par: ParentSchema): obj = {
    obj(
      "type" -> x.tpe.jsonType,
      "enum" -> arr(x.values.toSeq))
  }

  def mkOneOf(vv: Option[V.Def[_, _]], x: `oneof`[_], isRoot: Boolean, par: ParentSchema): obj = {
    val subTypesSeq = x.subTypes.toSeq
    val tpe = subTypesSeq.find {
      case `def`(_, _) => false
      case `ref`(_)    => false
      case _           => true
    }.map { _.jsonType }
    val sameType = subTypesSeq.tail.foldLeft(tpe.isDefined) {
      case (false, _) => false
      case (true, t)  => t.jsonType == tpe.get
    }
    def rootType = tpe filter { _ => !isRoot } map { t => obj("type"  -> `str`(t)) } getOrElse obj()
    if (sameType) {
      rootType ++ obj(
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = false, isRoot = false),
          subTypesSeq.tail.map(t => apply(t, Some(x), includeType = false, isRoot = false)): _*))
    } else
      obj(
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = true, isRoot = false),
          subTypesSeq.tail.map(apply(_, Some(x), includeType = true, isRoot = false)): _*))
  }

  def mkAllOf(vv: Option[V.Def[_, _]], x: `allof`[_], isRoot: Boolean, par: ParentSchema): obj = {
    val subTypesSeq = x.subTypes.toSeq
    val tpe = subTypesSeq.head.jsonType
    val sameType = subTypesSeq.tail.foldLeft(true) { case (agg, t) => agg && (t.jsonType == tpe) }
    if (sameType) {
      obj(
        "type"  -> (if (!isRoot) Some(`str`(tpe)) else None),
        "allOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = false, isRoot = false),
          subTypesSeq.tail.map(t => apply(t, Some(x), includeType = false, isRoot = false)): _*))
    } else
      obj(
        "allOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = true, isRoot = false),
          subTypesSeq.tail.map(apply(_, Some(x), includeType = true, isRoot = false)): _*))
  }


  def mkNot(vv: Option[V.Def[_, _]], x: `not`[_], par: ParentSchema): obj = {
    obj("not" -> apply(x.tpe, Some(x), includeType = false, isRoot = false))
  }

  def mkDef(vv: Option[V.Def[_, _]], x: `def`[_], par: ParentSchema): obj = {
    val ref = x.sig
    obj(f"$$ref" -> buildRef(ref))
  }

  def mkRef(vv: Option[V.Def[_, _]], x: `ref`[_], par: ParentSchema): obj = {
    val ref = x.sig
    obj(f"$$ref" -> buildRef(ref))
  }

  def buildRef(ref: String): String = s"#/definitions/$ref"

  def mkValueClass(vv: Option[V.Def[_, _]], x: `value-class`[_, _], par: ParentSchema): obj = {
    inferSpecifics.lift((vv, x.tpe, par, false)) getOrElse obj()
  }

  val inferSpecifics: PartialFunction[(Option[V.Def[_, _]], json.Schema[_], ParentSchema, Boolean), obj] = {
    case (vv, x: `string`[_], par, _)         => mkStr(vv, x, par)
    case (vv, x: `object`[_], par, _)         => mkObj(vv, x, par)
    case (vv, `dictionary`(comp), par, _)     => mkDict(vv, comp, par)
    case (vv, `array`(comp, unique), par, _)  => mkArr(vv, comp, unique, par)
    case (vv, x: `enum`[_], par, _)           => mkEnum(vv, x, par)
    case (vv, x: `oneof`[_], par, isRoot)     => mkOneOf(vv, x, isRoot, par)
    case (vv, x: `allof`[_], par, isRoot)     => mkAllOf(vv, x, isRoot, par)
    case (vv, x: `not`[_], par, _)            => mkNot(vv, x, par)
    case (vv, x: `def`[_], par, _)            => mkDef(vv, x, par)
    case (vv, x: `ref`[_], par, _)            => mkRef(vv, x, par)
    case (vv, x: `value-class`[_, _], par, _) => mkValueClass(vv, x, par)
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

  def inferDefinition(x: `def`[_], par: ParentSchema): (String, obj) = {
    val ref = x.sig
    ref -> apply(x.tpe, par orElse Some(x), includeType = true, isRoot = false)
  }

  def inferDefinitions(x: Schema[_]): obj = {
    def extractDefs(tpe: json.Schema[_], par: ParentSchema): Seq[(String, obj)] = tpe match {
      case x: `def`[_]          => extractDefs(x.tpe, par) :+ inferDefinition(x, par)
      case x: `allof`[_]        => x.subTypes.toSeq flatMap { extractDefs(_, Some(x)) }
      case x: `oneof`[_]        => x.subTypes.toSeq flatMap { extractDefs(_, Some(x)) }
      case x @ `array`(ref, _)  => extractDefs(ref, Some(x))
      case x @`dictionary`(ref) => extractDefs(ref, Some(x))
      case x @ `object`(fields) => fields.toSeq map { _.tpe } flatMap { extractDefs(_, Some(x)) }
      case _                    => Seq.empty
    }

    obj {
      extractDefs(x, None).toMap
    }
  }
}
