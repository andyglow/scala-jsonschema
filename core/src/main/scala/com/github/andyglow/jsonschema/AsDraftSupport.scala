package com.github.andyglow.jsonschema

import json._
import json.Schema._
import json.schema.{validation => V}
import com.github.andyglow.json.Value._
import json.Schema.`object`.Field.RWMode

import scala.collection.mutable.ListBuffer

trait AsDraftSupport {

  private[jsonschema] def isDraft04 = this.isInstanceOf[AsDraft04]

  private[jsonschema] def isDraft06 = this.isInstanceOf[AsDraft06]

  private[jsonschema] def isDraft07 = this.isInstanceOf[AsDraft07]

  private[jsonschema] def isDraft09 = this.isInstanceOf[AsDraft09]

  private[jsonschema] def isDraft12 = this.isInstanceOf[AsDraft12]

  type ParentSchema = Option[json.Schema[_]]

  /** Makes a definition signature a valid name for this draft (see [[DefinitionNames]]). */
  protected def sanitizeDefinitionName(sig: String): String

  // the names of the document being rendered; per thread (not inherited by threads started meanwhile), so one renderer can be shared, and nested `apply(x)` calls reuse the document's names
  private val definitionNames = new ThreadLocal[Map[String, String]]

  /** Renders `body` with the names of all definitions and refs of the document `root`, computed once before rendering it. */
  protected def withDefinitionNames[A](root: json.Schema[_])(body: => A): A =
    if (definitionNames.get != null) body
    else {
      definitionNames.set(DefinitionNames.assign(root, sanitizeDefinitionName))
      try body
      finally definitionNames.remove()
    }

  /** The name a definition signature has in the rendered document. */
  protected def definitionName(sig: String): String = Option(definitionNames.get).flatMap(_.get(sig)).getOrElse(sanitizeDefinitionName(sig))

  /** A tree that reaches a renderer with `option` nodes left (rendered without `AsValue.schema`, which lowers with the profile in scope) is rendered under the default profile. Idempotent. */
  protected def lowered(x: json.Schema[_]): json.Schema[_] = json.schema.JsonSchemaProfile.Neutral.lower(x)

  def apply(x: json.Schema[_]): obj = {
    val y = lowered(x)
    withDefinitionNames(y) {
      apply(y, None, includeType = true, isRoot = true)
    }
  }

  def apply(x: json.Schema[_], parent: ParentSchema, includeType: Boolean, isRoot: Boolean): obj = {
    val validationList = inferValidations(x)
    val specifics      = inferSpecifics.lift((validationList, x, parent, isRoot)) getOrElse obj.empty
    val base           = x match {
      case _: `def`[_] | _: `allof`[_] | _: `oneof`[_] | _: `ref`[_] => obj.empty
      case _: `const`[_]                                             => obj.empty // its value implies the type ("type": "const" before, in a mixed oneOf)
      case `value-class`(x) if includeType                           => obj("type" -> x.jsonType)
      case _ if includeType                                          => obj("type" -> x.jsonType)
      case _                                                         => obj.empty
    }

    base ++ validationList.json ++ specifics ++ deprecation(x)
  }

  /** `"deprecated": true` for a schema marked deprecated, from draft 2019-09 on (the keyword doesn't exist before) and in Raw. */
  private def deprecation(x: json.Schema[_]): obj = if (x.deprecated && this.isInstanceOf[Post09]) obj("deprecated" -> true) else obj.empty

  def mkStr(vl: ValidationList, x: `string`[_], par: ParentSchema): obj

  def mkObj(vl: ValidationList, x: `object`[_], par: ParentSchema): obj = {
    val discriminatorField = par match {
      case Some(`oneof`(_, f)) => f
      case _                   => None
    }
    val props = x.fields.map { field =>
      val default     = field.default map { d => obj("default" -> d) } getOrElse obj.empty
      val description = field.description map { d => obj("description" -> d) } getOrElse obj()
      val tpe         = apply(field.tpe, Some(x), includeType = true, isRoot = false)
      val rw          = field.rwMode match {
        case RWMode.ReadOnly  => obj("readOnly" -> true)
        case RWMode.WriteOnly => obj("writeOnly" -> true)
        case RWMode.ReadWrite => obj()
      }

      field.name -> (default ++
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
    } ++ discriminatorField.map(str.apply)

    val canHaveAdditionalProperties = x.isInstanceOf[`object`.Free]

    obj(
      ("description", x.description),
      ("title", x.title),
      ("additionalProperties", canHaveAdditionalProperties),
      ("properties", if (props.isEmpty) None else Some(obj(props))),
      ("required", if (required.isEmpty) None else Some(arr(required.distinct)))
    )
  }

  def mkDict(vl: ValidationList, comp: Schema[_], par: ParentSchema): obj = {
    val pattern = vl.extract(_.validation == V.Instance.`patternProperties`) map {
      _.json.asInstanceOf[str].value
    } getOrElse "^.*$"
    obj(
      "patternProperties" -> obj(
        pattern -> apply(comp, Some(comp), includeType = true, isRoot = false)
      )
    )
  }

  def mkArr(vl: ValidationList, comp: Schema[_], unique: Boolean, par: ParentSchema): obj = {
    obj(
      "items"       -> apply(comp, par, includeType = true, isRoot = false),
      "uniqueItems" -> (if (unique) Some(true) else None)
    )
  }

  def mkEnum(vl: ValidationList, x: `enum`[_], par: ParentSchema): obj = {
    obj("type" -> x.tpe.jsonType, "enum" -> arr(x.values.toSeq))
  }

  def mkOneOf(vl: ValidationList, x: `oneof`[_], isRoot: Boolean, par: ParentSchema): obj =
    if (x.subTypes.exists(_.isInstanceOf[`null`])) mkNullable(x) else mkPlainOneOf(x, isRoot)

  /** `oneof{X, null}`, which is what a profile makes of `Option[X]`: X's keywords with `"type": [X, "null"]` when X has a single JSON type (type arrays are valid from draft-04 on), otherwise
    * `oneOf[X, {"type": "null"}]` with each member keeping its own type: an `enum` must not take `null` from a type array, and keywords next to a `$ref` are ignored before 2019-09.
    */
  private def mkNullable(x: `oneof`[_]): obj = {
    def singleType(s: Schema[_]): Boolean = s match {
      case _: `boolean` | _: `integer` | _: `number`[_] | _: `string`[_] | _: `array`[_, _] | _: `object`[_] | _: `dictionary`[_, _, _] => true
      case `value-class`(t)                                                                                                             => singleType(t)
      case _                                                                                                                            => false
    }
    val subTypesSeq = x.subTypes.toSeq.sortBy(_.isInstanceOf[`null`]) // `null` last
    subTypesSeq match {
      case Seq(t, _: `null`) if singleType(t) =>
        obj("type" -> `arr`(`str`(t.jsonType), `str`("null"))) ++ apply(t, Some(x), includeType = false, isRoot = false)
      case _ =>
        obj("oneOf" -> `arr`(subTypesSeq.map(apply(_, Some(x), includeType = true, isRoot = false))))
    }
  }

  private def bodyType(d: `def`[_]): String = d.tpe match {
    case inner: `def`[_] => bodyType(inner)
    case `ref`(_)        => "" // unknown: not the same as any type
    case t               => memberType(t)
  }

  // a const's JSON type is its value's ("type": "const" was stated on a oneOf of consts before, which nothing matches)
  private def memberType(s: Schema[_]): String = s match {
    case `const`(v) => v.tpe
    case t          => t.jsonType
  }

  private def mkPlainOneOf(x: `oneof`[_], isRoot: Boolean): obj = {
    val subTypesSeq = x.subTypes.toSeq
    // each member's JSON type: a definition has its body's; a `$ref` to a definition elsewhere has none known here, so it doesn't count (asking either for `jsonType` threw before)
    val known = subTypesSeq collect {
      case d: `def`[_]                    => bodyType(d)
      case t if !t.isInstanceOf[`ref`[_]] => memberType(t)
    }
    // the type a same-typed oneOf states: the first member's that isn't a definition or a $ref, as in 0.7 (only definitions: none)
    val tpe = subTypesSeq.find {
      case `def`(_, _) | `ref`(_) => false
      case _                      => true
    }.map(memberType)
    val sameType = tpe.isDefined && known.forall(_ == tpe.get)
    def rootType = tpe filter { _ => !isRoot } map { t => obj("type" -> `str`(t)) } getOrElse obj()
    if (sameType) {
      rootType ++ obj(
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = false, isRoot = false),
          subTypesSeq.tail.map(t => apply(t, Some(x), includeType = false, isRoot = false)): _*
        )
      )
    } else
      obj(
        "oneOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = true, isRoot = false),
          subTypesSeq.tail.map(apply(_, Some(x), includeType = true, isRoot = false)): _*
        )
      )
  }

  def mkAllOf(vl: ValidationList, x: `allof`[_], isRoot: Boolean, par: ParentSchema): obj = {
    val subTypesSeq                  = x.subTypes.toSeq
    def isRef(s: Schema[_]): Boolean = s.isInstanceOf[`def`[_]] || s.isInstanceOf[`ref`[_]]
    val typed                        = subTypesSeq.filterNot(isRef) // a `$ref` has no type of its own (asking it threw before)
    val tpe                          = typed.headOption.fold("")(_.jsonType)
    val sameType                     = typed.nonEmpty && typed.forall(_.jsonType == tpe)
    if (sameType) {
      obj(
        "type"  -> (if (!isRoot) Some(`str`(tpe)) else None),
        "allOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = false, isRoot = false),
          subTypesSeq.tail.map(t => apply(t, Some(x), includeType = false, isRoot = false)): _*
        )
      )
    } else
      obj(
        "allOf" -> `arr`(
          apply(subTypesSeq.head, Some(x), includeType = true, isRoot = false),
          subTypesSeq.tail.map(apply(_, Some(x), includeType = true, isRoot = false)): _*
        )
      )
  }

  def mkNot(vl: ValidationList, x: `not`[_], par: ParentSchema): obj = {
    obj("not" -> apply(x.tpe, Some(x), includeType = false, isRoot = false))
  }

  def mkDef(vl: ValidationList, x: `def`[_], par: ParentSchema): obj = {
    val ref = definitionName(x.sig)
    obj(f"$$ref" -> buildRef(ref))
  }

  def mkRef(vl: ValidationList, x: `ref`[_], par: ParentSchema): obj = {
    val ref = definitionName(x.sig)
    obj(f"$$ref" -> buildRef(ref))
  }

  def mkConst(vl: ValidationList, x: `const`[_], par: ParentSchema): obj = {
    obj(
      ("description", x.description),
      ("title", x.title),
      ("const", x.value)
    )
  }

  def buildRef(ref: String): String = s"#/definitions/$ref"

  def mkValueClass(vl: ValidationList, x: `value-class`[_, _], par: ParentSchema): obj = {
    inferSpecifics.lift((vl, x.tpe, par, false)) getOrElse obj()
  }

  val inferSpecifics: PartialFunction[(ValidationList, json.Schema[_], ParentSchema, Boolean), obj] = {
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
    case (vv, x: `const`[_], par, _)          => mkConst(vv, x, par)
  }

  class ValidationList(elements: ListBuffer[V.Def[_, _]]) {

    def find(accept: V.Def[_, _] => Boolean): Option[V.Def[_, _]] = elements.find(accept)

    // tests all elements from left to right and
    // if matching found returns it, removing it from mutable elements collection at the same time
    def extract(accept: V.Def[_, _] => Boolean): Option[V.Def[_, _]] = {
      var res: Option[V.Def[_, _]] = None
      var i                        = 0
      while (res.isEmpty && i < elements.length) {
        if (accept(elements(i))) {
          res = Some(elements(i))
          elements.remove(i)
        }
        i = i + 1
      }

      res
    }

    def json: obj = obj {
      elements.map { d => d.validation.name -> d.json }.toMap
    }
  }

  def inferValidations(x: json.Schema[_]): ValidationList = {
//    import V.Instance._

//    val pp = x.validations.find(_.validation == `patternProperties`)
//    val validations = obj {
//      x.validations.collect {
//        case d if d.validation != `patternProperties` =>
//          d.validation.name -> d.json
//      }.toMap
//    }
//
//    (validations, pp)
    new ValidationList(ListBuffer.empty[V.Def[_, _]] ++= x.validations)
  }

  def inferDefinition(x: `def`[_], par: ParentSchema): (String, obj) = {
    val ref = definitionName(x.sig)
    ref -> apply(x.tpe, par orElse Some(x), includeType = true, isRoot = false)
  }

  def inferDefinitions(x: Schema[_]): obj = {
    def extractDefs(tpe: json.Schema[_], par: ParentSchema): Seq[(String, obj)] = tpe match {
      case x: `def`[_] =>
        extractDefs(x.tpe.withValidationsAddedFrom(x), par) :+ inferDefinition(x, par)
      case x: `allof`[_]         => x.subTypes.toSeq flatMap { extractDefs(_, Some(x)) }
      case x: `oneof`[_]         => x.subTypes.toSeq flatMap { extractDefs(_, Some(x)) }
      case x @ `array`(ref, _)   => extractDefs(ref, Some(x))
      case x @ `dictionary`(ref) => extractDefs(ref, Some(x))
      case x @ `object`(fields)  => fields.toSeq map { _.tpe } flatMap { extractDefs(_, Some(x)) }
      case _                     => Seq.empty
    }

    obj {
      extractDefs(x, None).toMap
    }
  }
}
