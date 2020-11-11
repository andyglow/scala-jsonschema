package com.github.andyglow.jsonschema

import com.github.andyglow.scaladoc.{Scaladoc, SlowParser}

import scala.reflect.internal.util.NoSourceFile


private[jsonschema] trait UScaladocs { this: UContext with UCommons =>
  import c.universe._

  def getTypeScaladoc(tpe: Type): Option[Scaladoc] = {
    import com.github.andyglow.scalamigration._

    val pos = tpe.typeSymbol.pos
    pos.source match {
      case NoSourceFile => None
      case src =>
        val str = new String(src.content, 0, src.lineToOffset(pos.line - 1)).trim
        if (str.endsWith("*/")) {
          val start = str.lastIndexOf("/**")
          if (start >= 0) {
            SlowParser.parse(str.substring(start)).opt
          } else None
        } else None
    }
  }
}