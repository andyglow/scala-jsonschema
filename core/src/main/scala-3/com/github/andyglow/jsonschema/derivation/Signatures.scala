package com.github.andyglow.jsonschema.derivation

/** Type signatures (`Json.sig`, definition and `$ref` names), spelled exactly like Scala 2's `USignatures` (inventory §6.1-6.2):
  *   - full names with `.` separators: no `$` of module classes; owners that aren't classes or packages (methods, local blocks) and package objects (incl. Scala 3's `<file>$package`) skipped;
  *   - name segments are decoded (`odd name`, `+`): owner decision 2026-11-20, `Json.sig` stays readable and the renderers sanitize names per draft (Scala 2 decodes too since s2fix);
  *   - a compound type is its parents' signatures joined by ` with `; an annotated type is its underlying type's signature;
  *   - type arguments in brackets, no spaces: `scala.Predef.Map[scala.Predef.String,scala.Int]`;
  *   - aliases are kept (`scala.Predef.String`), no `.type` for singletons, constants print their value.
  *
  * Scala 3 eagerly replaces *applied* standard-library aliases by their class (`List[Int]` arrives as `scala.collection.immutable.List[scala.Int]`), so the user's spelling is lost. [[Scala2Spelling]]
  * maps those classes back to the alias Scala 2 prints for the usual spelling (`scala.List`, `scala.Predef.Map`, ...). It is not applied to the top level of a dealiased type (the derivation dealiases
  * every type it resolves, like Scala 2, so it prints the class there; only `Json.sig` keeps the user's alias). Limitation: the class can't be told apart from its alias, so an explicitly spelled
  * `Json.sig[scala.collection.immutable.List[Int]]` prints `scala.List[scala.Int]` (Scala 2: the full name).
  */
trait Signatures { this: Derivation =>
  import q.reflect.*

  /** The signature of `tpe`.
    * @param dealiased
    *   `tpe` was dealiased at the top level (every type the derivation resolves; not `Json.sig`): keep the class name of an applied alias
    */
  def signature(tpe: TypeRepr, dealiased: Boolean = false): String = sig(tpe, mapAlias = !dealiased)

  private def sig(tpe: TypeRepr, mapAlias: Boolean): String = tpe match {
    case ConstantType(c)       => String.valueOf(c.value)
    case t: TermRef            => fullName(t.termSymbol)
    case AppliedType(tc, args) => tyconName(tc, mapAlias) + args.map(sig(_, mapAlias = true)).mkString("[", ",", "]")
    case t: TypeRef            => fullName(t.typeSymbol)
    case t: ThisType           => fullName(t.typeSymbol)
    case t: ParamRef           => t.show
    case AnnotatedType(u, _)   => sig(u, mapAlias)
    case AndType(a, b)         => sig(a, mapAlias) + " with " + sig(b, mapAlias)
    case TypeBounds(_, _)      => "_"
    case t                     => unsupported(t)
  }

  private def unsupported(t: TypeRepr): Nothing =
    report.errorAndAbort(s"schema for ${t.show} is not supported: a definition can't be named after this type (a structural or other unnamed type). Use a named type, e.g. a case class")

  private def tyconName(tc: TypeRepr, mapAlias: Boolean): String = {
    val name = tc match {
      case t: TypeRef => fullName(t.typeSymbol)
      case t          => sig(t, mapAlias = true)
    }
    if (mapAlias) Scala2Spelling.getOrElse(name, name) else name
  }

  /** Scala 2 `Symbol.fullName`, decoded. */
  def fullName(sym: Symbol): String = {
    def isRoot(s: Symbol) = s.isNoSymbol || s == defn.RootClass || s == defn.RootPackage || s.name == "<empty>" || s.name == "<root>"
    // Scala 2 skips package objects (`scala.BigDecimal`, not `scala.package.BigDecimal`); Scala 3 also has `<file>$package` objects for top-level definitions
    def isPackageObject(s: Symbol)      = { val n = s.name.stripSuffix("$"); n == "package" || n.endsWith("$package") }
    def segment(s: Symbol): String      = s.name.stripSuffix("$")
    def owners(s: Symbol): List[String] =
      if (isRoot(s)) Nil
      else if ((s.isClassDef || s.isPackageDef) && !isPackageObject(s)) owners(s.owner) :+ segment(s)
      else owners(s.owner)
    if (isRoot(sym)) sym.name else (owners(sym.owner) :+ segment(sym)).mkString(".")
  }

  /** Class full name -> Scala 2's spelling of the usual alias for it. */
  private val Scala2Spelling: Map[String, String] = Map(
    "scala.collection.immutable.List"         -> "scala.List",
    "scala.collection.immutable.$colon$colon" -> "scala.$colon$colon",
    "scala.collection.immutable.Seq"          -> "scala.Seq",
    "scala.collection.immutable.IndexedSeq"   -> "scala.IndexedSeq",
    "scala.collection.immutable.Vector"       -> "scala.Vector",
    "scala.collection.immutable.LazyList"     -> "scala.LazyList",
    "scala.collection.immutable.Stream"       -> "scala.Stream",
    "scala.collection.immutable.Map"          -> "scala.Predef.Map",
    "scala.collection.immutable.Set"          -> "scala.Predef.Set",
    "scala.collection.Iterable"               -> "scala.Iterable",
    "scala.collection.Iterator"               -> "scala.Iterator",
    "scala.collection.IterableOnce"           -> "scala.IterableOnce",
    "scala.util.Either"                       -> "scala.Either",
    "scala.util.Left"                         -> "scala.Left",
    "scala.util.Right"                        -> "scala.Right"
  )
}
