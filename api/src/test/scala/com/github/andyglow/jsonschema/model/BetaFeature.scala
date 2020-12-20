package com.github.andyglow.jsonschema.model

import json.schema.typeHint

@typeHint[String] sealed trait BetaFeature

case object F0 extends BetaFeature
case object F1 extends BetaFeature
case object F2 extends BetaFeature
