package com.github.andyglow.jsonschema

// Scala 3's order of the members of a sealed family compiled in another module (`testmodels.LibraryColor`): declaration
// order, read from TASTy. Scala 2: by name.
object EnumOrderExpected {
  val libraryColor: List[String] = List("Red", "Green", "Blue", "Cyan", "Yellow", "Black")
}
