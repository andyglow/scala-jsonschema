- **0.6.3**
  - fixed scala.None handling for the default values of case class parameters (scala 2.11 is not supported this kind of functionality)
- **0.6.2**
  - added annotations for
    - definition enforcement
    - discriminator/discriminatorKey  
- **0.6.1**
  - added support for scala.Enumeration
  - default values won't be infered for scala.None
- **0.6.0**
  ...
- **0.2.7-M1**
  - Add initial code for supporting #18. Use scaladoc for descriptions. 
    Added support for case classes and fields.
  - todo:
    - add scaladoc support for sum types
    - here is some problem. comments live in source files and get erased after compilation, so resulted bytecode 
      doesn't have this info and thus schemas derived from such models won't have descriptions.
      this might probably be solved by packing scaladocs into annotations by macro or compiler plugin and
      have those reused my SchemaMacro. Here is an example of something like this: 
      https://medium.com/@takezoe/a-compiler-plugin-makes-scaladoc-readable-at-runtime-aecbebccb794 
- **0.2.6**
  - hot-fix release. disable mistakingly left enabled debug mode for Refined macro
- **0.2.5**
  - Improve Schema matching. 
    Since now Ref and Validations are taken into an account during schema comparision.
  - Cats version upgraded to 2.1.1 for scala 2.12 and greater  
  - Support Refined types (0.9.13 for scala 2.12, 2.13, 0.9.12 for scala 2.11) 
- **0.2.4**
  - Support Cats data types (non-empty-*)
  - Support generic product types
  - Module `scala-jsonschema-api` renamed into `scala-jsonschema`
- **0.2.3**
  - Support generic sum types (aka sealed trait hierarchies)
  