package com.github.andyglow.jsonschema.derivation

/** Type signatures (`Json.sig`, definition and `$ref` names), spelled exactly like Scala 2's `USignatures` (inventory §6.1-6.2):
  *   - full names with `.` separators: no `$` of module classes; owners that aren't classes or packages (methods, local blocks) and package objects (incl. Scala 3's `<file>$package`) skipped;
  *   - name segments are decoded (`odd name`, `+`): owner decision 2026-11-20, `Json.sig` stays readable and the renderers sanitize names per draft (Scala 2 decodes too since s2fix);
  *   - a compound type is its parents' signatures joined by ` with `; an annotated type is its underlying type's signature;
  *   - type arguments in brackets, no spaces: `scala.Predef.Map[scala.Predef.String,scala.Int]`;
  *   - aliases are kept (`scala.Predef.String`), no `.type` for singletons, constants print their value.
  *
  * Scala 3 eagerly reduces *applied* eta-reducible aliases (`List[Int]` arrives as `scala.collection.immutable.List[scala.Int]`), so the alias's name is lost. [[Scala2Spelling]] maps those classes
  * back to the alias Scala 2 prints for the usual spelling (`scala.List`, `scala.Predef.Map`, ...). In `Json.sig` only when the type constructor was reached through an alias ([[viaAlias]]): a
  * spelled-out `Json.sig[scala.collection.immutable.List[Int]]` keeps its class name, as in Scala 2. In definition names always (in type arguments), because the alias signal is lost in TASTy: a
  * spelled-out `immutable.List` in a field type's argument prints `scala.List` there (Scala 2: the class name). It is not applied to the top level of a dealiased type (the derivation dealiases every
  * type it resolves, like Scala 2, so it prints the class there; only `Json.sig` keeps the user's alias). Known difference: a user's own eta-reducible alias (`type MyList[A] = List[A]`,
  * `type GInt[A] = G[A]`) prints as what it reduces to; an alias that isn't eta-reducible (`type StrMap[V] = Map[String, V]`) is kept.
  */
trait Signatures { this: Derivation =>
  import q.reflect.*

  /** The signature of `tpe`.
    * @param dealiased
    *   `tpe` was dealiased at the top level (every type the derivation resolves; not `Json.sig`): keep the class name of an applied alias
    */
  def signature(tpe: TypeRepr, dealiased: Boolean = false): String = sig(tpe, mapAlias = !dealiased, spelled = !dealiased)

  /** @param mapAlias
    *   apply [[Scala2Spelling]] to this type's constructor (not at the top level of a dealiased type)
    * @param spelled
    *   `tpe` is the call-site type of `Json.sig`, typed from source in this run: map only constructors reached through an alias ([[viaAlias]]). Definition names map every standard class: their types
    *   may come from TASTy (a model compiled in another module), where the alias signal isn't there, and the name must not depend on that (reviewer4 B1)
    */
  private def sig(tpe: TypeRepr, mapAlias: Boolean, spelled: Boolean): String = tpe match {
    case ConstantType(c)       => String.valueOf(c.value)
    case t: TermRef            => fullName(t.termSymbol)
    case AppliedType(tc, args) => tyconName(tc, mapAlias, spelled) + args.map(sig(_, mapAlias = true, spelled)).mkString("[", ",", "]")
    case t: TypeRef            => fullName(t.typeSymbol)
    case t: ThisType           => fullName(t.typeSymbol)
    case t: ParamRef           => t.show
    case AnnotatedType(u, _)   => sig(u, mapAlias, spelled)
    case AndType(a, b)         => sig(a, mapAlias, spelled) + " with " + sig(b, mapAlias, spelled)
    case TypeBounds(_, _)      => "_"
    case t                     => unsupported(t)
  }

  private def unsupported(t: TypeRepr): Nothing =
    report.errorAndAbort(s"schema for ${t.show} is not supported: a definition can't be named after this type (a structural or other unnamed type). Use a named type, e.g. a case class")

  private def tyconName(tc: TypeRepr, mapAlias: Boolean, spelled: Boolean): String = tc match {
    case t: TypeRef =>
      val name = fullName(t.typeSymbol)
      if (mapAlias && (!spelled || viaAlias(t))) Scala2Spelling.getOrElse(name, name) else name
    case t => sig(t, mapAlias = true, spelled)
  }

  /** The type constructor was reached by reducing an alias: its prefix is the `this` of the alias's right-hand side (`scala.List[Int]` arrives as `immutable.this.List[Int]`). A path the user wrote
    * (`scala.collection.immutable.List[Int]`, or an imported `immutable.Map`) keeps a term prefix, and Scala 2 prints the class name for it too. Which alias it was is lost, so a user-defined
    * eta-reducible alias (`type MyList[A] = List[A]`) is printed as the standard one (`scala.List`); Scala 2 prints `MyList`. Only for types typed from source in this run: a type read from TASTy has
    * a term prefix either way.
    */
  private def viaAlias(t: TypeRef): Boolean = t.qualifier match {
    case _: ThisType => true
    case _           => false
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
