package com.github.andyglow.jsonschema

import scaladoc._
import scaladoc.macros.ExtractScaladoc


private[jsonschema] trait UScaladocs extends ExtractScaladoc { this: UContext with UCommons =>
  import c.universe._

  def getTypeScaladoc(tpe: Type): Option[Scaladoc] = {
    fromAttachment orElse fromAnnotatedType(tpe) orElse fromSourceCode(tpe.typeSymbol.pos)
  }
}