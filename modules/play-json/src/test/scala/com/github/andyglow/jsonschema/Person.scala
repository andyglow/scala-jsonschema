package com.github.andyglow.jsonschema

import play.api.libs.json.JsObject

case class Person(id: String, name: String, metadata: JsObject)
