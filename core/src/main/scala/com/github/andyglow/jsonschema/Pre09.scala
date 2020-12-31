package com.github.andyglow.jsonschema

import json.schema.{validation => V}
import com.github.andyglow.json.Value.obj
import json.Schema.`string`

trait Pre09 { this: AsDraftSupport =>

  /** Adds `format` attribute if Format is defined and it defines formats known prior to 2012-09.
    * In case it's `uuid` and `pattern` validation is not defined manually, it will add pattern attribute.
    */
  def mkStr(vl: ValidationList, x: `string`[_], par: ParentSchema): obj = {
    import `string`.Format._

    val format = x.format.fold(obj.empty) {
      case `duration`      => obj.empty
      case `uuid`          => obj.empty
      case `idn-hostname`  => obj.empty
      case f               => obj("format" -> f.productPrefix)
    }

    val pattern = vl.find(_.validation == V.Instance.`pattern`)
    val extraVV = x.format flatMap {
      case `uuid` if pattern.isEmpty => Some(V.Instance.`pattern` := Constants.RegEx.uuid)
      case _                         => None
    }

    format ++ extraVV.fold(obj.empty) { extraVV =>
      obj(extraVV.validation.name -> extraVV.json)
    }
  }
}
