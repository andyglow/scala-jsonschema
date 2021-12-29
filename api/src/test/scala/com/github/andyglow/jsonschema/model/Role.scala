package com.github.andyglow.jsonschema.model

import json.schema.typeHint

// members goes inside companion

@typeHint[String]
sealed trait Role

object Role {

  case object User extends Role

  case object Admin extends Role

  case object Manager extends Role
}
