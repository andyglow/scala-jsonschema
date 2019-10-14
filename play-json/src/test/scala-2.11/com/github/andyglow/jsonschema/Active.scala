package com.github.andyglow.jsonschema

import com.github.andyglow.json.{ToValue, Value}

// members goes without companion

sealed trait Active

case object On extends Active
case object Off extends Active
case object Suspended extends Active

object Active {

  implicit val ActiveV: ToValue[Active] = ToValue mk {
    case On         => Value.str("On")
    case Off        => Value.str("Off")
    case Suspended  => Value.str("Suspended")
  }
}