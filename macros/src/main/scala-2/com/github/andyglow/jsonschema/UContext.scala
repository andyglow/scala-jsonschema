package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

private[jsonschema] trait UContext { val c: blackbox.Context }
