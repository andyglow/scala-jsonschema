package com.github.andyglow.jsonschema.model

import com.github.andyglow.json.{ToValue, Value}
import json.schema.typeHint

// members goes without companion

@typeHint[String]
sealed trait Active

case object On        extends Active
case object Off       extends Active
case object Suspended extends Active

object Active {

  // ToValue is explicitly specified
  implicit val ActiveV: ToValue[Active] = ToValue mk {
    case On        => Value.str("On")
    case Off       => Value.str("Off")
    case Suspended => Value.str("Suspended")
  }
}
