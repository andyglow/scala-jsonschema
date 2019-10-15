package com.github.andyglow.jsonschema.model


// members goes inside companion

sealed trait Role

object Role {

  case object User extends Role

  case object Admin extends Role

  case object Manager extends Role
}