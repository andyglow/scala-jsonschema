package com.github.andyglow.jsonschema.profiles

/** Scala 2: circe-generic-extras' `Configuration` isn't a dependency, so only the semiauto profile is implicit. */
trait CirceProfiles extends CirceLowPriorityProfiles
