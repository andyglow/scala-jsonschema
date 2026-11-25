package com.github.andyglow.jsonschema

/** Semi-auto derivation (issue #542): the modes, the gate by kind, the phase-1 warning and the error, and what every expansion adds around its output. */
private[jsonschema] trait USemiAuto { this: UContext with UCommons with UFlags with USignatures =>
  import c.universe._

  /** The root is `Json.auto.derived`: every nested type is derived in place (auto mode for the whole expansion). */
  protected var autoRoot: Boolean = false

  private var warned = List.empty[Type]

  private def isTuple(sym: Symbol): Boolean      = definitions.TupleClass.seq.contains(sym)
  private lazy val eitherSyms: Set[Symbol]       = Set(typeOf[Either[_, _]], typeOf[Left[_, _]], typeOf[Right[_, _]]).map(_.typeSymbol)
  private def isCollectionLike(t: Type): Boolean =
    t <:< T.anyMap || t <:< T.jMap || t <:< T.array || t <:< T.iterable || t <:< T.jCollection || T.isOptionLike(t)

  /** A sealed type with a case class among its leaves (nested sealed families flattened). An all-case-object family is an enum. */
  private def hasCaseClassCase(sym: ClassSymbol): Boolean = sym.knownDirectSubclasses.exists { s =>
    s.isClass && (if (s.asClass.isSealed) hasCaseClassCase(s.asClass) else s.asClass.isCaseClass && !s.isModuleClass)
  }

  /** The kinds semi-auto doesn't derive behind the user's back: non-tuple case classes, and sealed families with a case class case (other than `Either`). Value classes, case-object enums, Java enums,
    * `Enumeration`, tuples, `Either`, `Option`/`Optional`, collections and maps are native.
    */
  def isGatedKind(tpe: Type): Boolean = {
    val sym = tpe.typeSymbol
    if (!sym.isClass || sym.isModuleClass || isCollectionLike(tpe) || eitherSyms.contains(sym) || isTuple(sym)) false
    else {
      val cls = sym.asClass
      if (cls.isJavaEnum) false
      else if (cls.isSealed) hasCaseClassCase(cls)
      else cls.isCaseClass && !cls.isDerivedValueClass
    }
  }

  /** A case (or nested sub-family) of a sealed family being derived: derived as part of it, as in circe. */
  def isCaseOfFamilyOnStack(tpe: Type, stack: List[Type]): Boolean = {
    val bases = tpe.baseClasses.drop(1).toSet
    stack.exists(s => isSealed(s) && bases.contains(s.typeSymbol))
  }

  /** `addressSchema` for `Address` (Scala 3: `Errors.valName`). */
  private def valName(tpe: Type): String = {
    val n = tpe.dealias.typeSymbol.name.decodedName.toString.filter(_.isLetterOrDigit)
    (if (n.isEmpty) "x" else s"${n.head.toLower}${n.tail}") + "Schema"
  }

  /** `Address`, `Box[P]`: as the user would write it with the type imported. */
  private def shortType(tpe: Type): String = tpe.dealias match {
    case TypeRef(_, sym, Nil)  => sym.name.decodedName.toString
    case TypeRef(_, sym, args) => sym.name.decodedName.toString + args.map(shortType).mkString("[", ", ", "]")
    case other                 => other.toString
  }

  // the same texts as Scala 3's `Errors.semiAutoFixes`
  private def fixes(tpe: Type): String = {
    val short = shortType(tpe)
    s"Define one: `implicit val ${valName(tpe)}: json.Schema[$short] = json.Json.schema[$short]` (a definition + $$ref; for a generic type " +
      "`implicit def boxSchema[T](implicit p: json.schema.Predef[T]): json.Schema[Box[T]] = json.Json.schema[Box[T]]`),\n" +
      "or derive nested types automatically: `import json.Json.auto._` (Scala 3: `import json.Json.auto.given`)"
  }

  /** A gated nested type without an instance, in semi-auto mode: the error with `Flag.SemiAuto`, else (phase 1) the warning, and the type is derived in place as before. The warning comes only from an
    * expansion that isn't itself inside an implicit search (whose candidates may be discarded or tried again).
    */
  def semiAutoGate(tpe: Type, stack: List[Type]): Unit =
    if (flags.semiAuto)
      c.abort(
        c.enclosingPosition,
        s"schema for ${showType(tpe)} is not supported${pathOf(stack)}: no implicit json.Schema or json.schema.Predef for it.\n${fixes(tpe)}"
      )
    else if (!insideImplicitSearch && !warned.exists(_ =:= tpe)) {
      warned = tpe :: warned
      c.warning(
        c.enclosingPosition,
        s"[json-schema-semi-auto] schema for ${showType(tpe)} is derived implicitly${pathOf(stack)}; this becomes an error in a future release. ${fixes(tpe)}"
      )
    }

  /** `Schema.building(sig)(out)`: an instance of a type met while this schema is built that refers back to it (a recursion cycle between instances) is referenced lazily. Only for a root derived here
    * (not an instance found for it), of a class type with a compile-time name.
    */
  def buildingRoot(tpe0: Type, out: Tree, rootFromImplicit: Boolean, requireObject: Boolean): Tree = {
    val tpe   = tpe0.dealias
    val named = tpe match {
      case TypeRef(_, sym, _) => sym.isClass && !mentionsAbstractType(tpe)
      case _                  => false
    }
    if (rootFromImplicit || !named) out
    else {
      val built = q"${N.Schema}.building[$tpe](${signature(tpe)})($out)"
      if (requireObject) q"$built.asInstanceOf[${tq"${N.Schema}.`object`[$tpe]"}]" else built
    }
  }

  // `openImplicits` is on the whitebox API only; every macro context implements it
  private def insideImplicitSearch: Boolean = c.asInstanceOf[scala.reflect.macros.whitebox.Context].openImplicits.nonEmpty

  private val markerName = TermName("jsonSchemaAutoEnabled")

  /** A use of the marker `import json.Json.auto._` brings (`auto.jsonSchemaAutoEnabled`), so the import counts as used (design A.4). Scala 2 counts an imported implicit that the macro's own search
    * selected as a use (spike 0(d)), but not one the search only tried or declined: `Json.schema[Int]` with the import (2.13), a self reference hiding the bridge (`implicit val p: Predef[A] =
    * Predef(Json.schema[A])`, 2.12). So the marker goes into every expansion that sees the import, not only those that took a type from it: the import puts the whole expansion in auto mode. Spliced
    * only if the name resolves to the marker (an import of `auto.derived` alone, or a local of that name: nothing).
    */
  def autoMarker(out: Tree): Tree =
    c.typecheck(Ident(markerName), silent = true) match {
      case t if t.symbol != null && t.symbol != NoSymbol && t.symbol.fullName == "json.Json.auto.jsonSchemaAutoEnabled" => q"{ val _ = ${Ident(markerName)}; $out }"
      case _                                                                                                            => out
    }
}
