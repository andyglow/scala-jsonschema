package com.github.andyglow.jsonschema.model

/** `Option` defaults, for the bridge specs (a `Some(..)` default needs the bridge's `ToValue`; Scala 2 reads it only from a separately compiled model, see FieldsCharSpec F4). */
case class WithOptionDefaults(some: Option[Int] = Some(5), none: Option[Int] = None)
