package petshop

/** What each library writes for [[Samples.pets]] (Rex the dog with an age, Nemo the fish without one), for the cross-check: every profile against every library's output. Each bridge module's
  * acceptance spec checks that its library really writes its entry (as JSON, key order aside), so these stay true.
  */
object LibraryOutputs {

  val circeSemiauto: List[String]   = List("""{"name":"Rex","age":3,"animal":{"Dog":{"name":"Rex"}}}""", """{"name":"Nemo","age":null,"animal":{"Fish":{}}}""")
  val circeConfigured: List[String] = List("""{"name":"Rex","age":3,"animal":{"name":"Rex","type":"Dog"}}""", """{"name":"Nemo","age":null,"animal":{"type":"Fish"}}""")
  val playDefault: List[String]     = List("""{"name":"Rex","age":3,"animal":{"_type":"petshop.Dog","name":"Rex"}}""", """{"name":"Nemo","animal":{"_type":"petshop.Fish"}}""")
  val playKind: List[String]        = List("""{"name":"Rex","age":3,"animal":{"_kind":"Dog","name":"Rex"}}""", """{"name":"Nemo","age":null,"animal":{"_kind":"Fish"}}""")
  val upickle: List[String]         = List("""{"name":"Rex","age":3,"animal":{"$type":"Dog","name":"Rex"}}""", """{"name":"Nemo","age":null,"animal":"Fish"}""")
  val json4s: List[String]          = List("""{"name":"Rex","age":3,"animal":{"name":"Rex"}}""", """{"name":"Nemo","animal":{}}""")
  val json4sHints: List[String]     = List("""{"name":"Rex","age":3,"animal":{"kind":"Dog","name":"Rex"}}""", """{"name":"Nemo","animal":{"kind":"Fish$"}}""")
  val spray: List[String]           = List("""{"age":3,"animal":{"name":"Rex","type":"Dog"},"name":"Rex"}""", """{"animal":{"type":"Fish"},"name":"Nemo"}""")

  val all: List[(String, List[String])] = List(
    "circe semiauto"   -> circeSemiauto,
    "circe configured" -> circeConfigured,
    "play default"     -> playDefault,
    "play _kind"       -> playKind,
    "upickle"          -> upickle,
    "json4s"           -> json4s,
    "json4s hints"     -> json4sHints,
    "spray (AnimalF)"  -> spray
  )
}
