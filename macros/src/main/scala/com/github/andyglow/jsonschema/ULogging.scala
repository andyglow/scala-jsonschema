package com.github.andyglow.jsonschema

private[jsonschema] trait ULogging { this: UContext =>

  val debugEnabled = false

  val dbg: String => Unit =
    if (debugEnabled) c.info(c.enclosingPosition, _, force = true) else _ => ()

  val info: String => Unit = c.info(c.enclosingPosition, _, force = true)

  val warn: String => Unit = c.warning(c.enclosingPosition, _)

  val err: String => Unit = c.error(c.enclosingPosition, _)

  val abort: String => Nothing = c.abort(c.enclosingPosition, _)
}
