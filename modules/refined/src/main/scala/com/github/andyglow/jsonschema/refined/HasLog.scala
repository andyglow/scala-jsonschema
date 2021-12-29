package com.github.andyglow.jsonschema.refined

private[jsonschema] trait HasLog { this: HasContext =>
  val debugEnabled = false

  val dbg: String => Unit =
    if (debugEnabled) c.info(c.enclosingPosition, _, force = true) else _ => ()

  val warn: String => Unit = c.warning(c.enclosingPosition, _)

  val err: String => Nothing = c.abort(c.enclosingPosition, _)
}
