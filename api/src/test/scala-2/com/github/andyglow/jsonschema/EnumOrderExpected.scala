package com.github.andyglow.jsonschema

// Scala 2's order of the members of a sealed family compiled in another module (`testmodels.LibraryColor`): by name,
// because members loaded from class files have no source positions (UCommons.inDeclarationOrder). Scala 3: declaration order.
object EnumOrderExpected {
  val libraryColor: List[String] = List("Black", "Blue", "Cyan", "Green", "Red", "Yellow")
}
