package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value.obj
import json.Schema.`string`

trait Post09 { this: AsDraftSupport =>

  def mkStr(vl: ValidationList, x: `string`[_], par: ParentSchema): obj =
    obj("format" -> x.format.map(_.productPrefix))
}
