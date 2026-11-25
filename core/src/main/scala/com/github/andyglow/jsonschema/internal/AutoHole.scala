package com.github.andyglow.jsonschema.internal

import json.schema.Predef

/** INTERNAL API (issue #542): what `Json.auto.derived[X]` expands to while a derivation searches for an implicit. The derivation replaces every hole it gets back with the schema of `X` from its own
  * expansion (one recursion stack, so a recursive `X` is a `$ref`), and checks that none is left in its output. Evaluating one is a bug.
  */
object AutoHole {

  def hole[T]: Predef[T] = throw new IllegalStateException("json-schema: an auto-derivation placeholder was evaluated (a bug in the derivation, please report it)")
}

/** INTERNAL API (issue #542): the marker `import json.Json.auto._` brings into scope. Every auto-mode expansion refers to it, so the import counts as used. */
final class AutoEnabled private[jsonschema] ()
object AutoEnabled {
  val instance: AutoEnabled = new AutoEnabled
}
