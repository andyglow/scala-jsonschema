package com.github.andyglow.jsonschema.refined

import scala.reflect.macros.blackbox

private[jsonschema] trait HasContext {

  val c: blackbox.Context

}
