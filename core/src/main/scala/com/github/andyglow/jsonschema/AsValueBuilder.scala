package com.github.andyglow.jsonschema

import json.schema.Version
import json.schema.Version._


trait AsValueBuilder[V <: Version] {

  def apply(v: V): AsValue
}

trait LowPriorityAsValueBuilder {

  implicit val pseudo: AsValueBuilder[Raw.type] = new AsValueBuilder[Raw.type] {
    override def apply(v: Raw.type): AsValue = AsRaw
  }

  implicit val draft04: AsValueBuilder[Draft04] = new AsValueBuilder[Draft04] {
    override def apply(v: Draft04): AsValue = new AsDraft04(v)
  }

  implicit val draft06: AsValueBuilder[Draft06] = new AsValueBuilder[Draft06] {
    override def apply(v: Draft06): AsValue = new AsDraft06(v)
  }

  implicit val draft07: AsValueBuilder[Draft07] = new AsValueBuilder[Draft07] {
    override def apply(v: Draft07): AsValue = new AsDraft07(v)
  }
}

object AsValueBuilder extends LowPriorityAsValueBuilder