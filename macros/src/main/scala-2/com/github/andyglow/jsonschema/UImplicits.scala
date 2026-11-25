package com.github.andyglow.jsonschema

private[jsonschema] trait UImplicits { this: UContext with UCommons with USignatures =>
  import c.universe._

  class Implicit {
    sealed trait ImplicitSchema
    case class FromPredef(x: Tree) extends ImplicitSchema
    case class FromSchema(x: Tree) extends ImplicitSchema
    case object NotFound           extends ImplicitSchema

    // the symbols whose right-hand side contains this expansion: the val, lazy val or def being defined and its owners
    private lazy val enclosingSymbols: Set[Symbol] = {
      def loop(s: Symbol, acc: Set[Symbol]): Set[Symbol] = if (s == NoSymbol || s.isPackageClass) acc else loop(s.owner, acc + s)
      loop(c.internal.enclosingOwner, Set.empty)
    }

    // A self reference: `implicit val s: Schema[X] = Json.schema[X]` (or a Predef, a def, ...). A plain search for Schema[X] inside the right-hand side finds `s` itself, which is
    // still null when the derived code runs (a def recurses forever), so the macro declines it. But scalac's `-Xlint:implicit-recursion` (2.13) reports the search as soon as it
    // finds `s` (#320). So on 2.13.17+ the search leaves the enclosing definitions out (`ImplicitSearch`), and `unlessSelfWins` keeps the result only where today's search
    // would have picked it; older compilers search the plain way and decline afterwards (`isOwnerSelfRef`).

    // what a search can return for an enclosing definition: the val/def itself, a field's getter, an object's module
    private lazy val selfSymbols: Seq[Symbol] = enclosingSymbols.toSeq.flatMap { s =>
      if (s.isTerm && !s.isMethod && s.asTerm.getter != NoSymbol) Seq(s, s.asTerm.getter)
      else if (s.isModuleClass) Seq(s, s.asClass.module)
      else Seq(s)
    }

    // the simple names of the enclosing vals/defs (a field is named by its getter), to shadow them in a typechecked search
    private lazy val selfNames: Seq[TermName] = selfSymbols.collect {
      case s if s.isTerm && !s.isConstructor && !s.isModule && { val n = s.name.decodedName.toString; n.nonEmpty && !n.exists(ch => ch == ' ' || ch == '$' || ch == '<') } =>
        s.name.toTermName
    }

    private def infer(pt: Type): Tree = ImplicitSearch.inferIgnoring(c)(pt, selfSymbols) match {
      case Some(y) => unlessSelfWins(pt, y)
      case None    => c.inferImplicitValue(pt)
    }

    // Today's winner without the self reference. The plain search finds the enclosing implicit (it is always in the lexical scope), and the macro declines it. A search
    // without it returns the next candidate, which may have lost to it: an implicit-scope one (companions: scalac only looks there when the lexical scope has nothing) or a
    // lexical one it beats. Keep `y` only if it would have won against every enclosing candidate, as scalac ranks them (Implicits.improves, Infer.isStrictlyMoreSpecific);
    // otherwise it's a declined self reference, as before (EmptyTree: the Schema search goes on to Predef, the Predef search to derivation).
    // Known corners of the replay: an implicit imported from an object of `pt`'s implicit scope counts as implicit scope (declined), and `-Xsource:3`'s variance flip in
    // `isAsSpecificValueType` isn't replayed (Schema and Predef are covariant).
    private def unlessSelfWins(pt: Type, y: Tree): Tree =
      if (y == EmptyTree) y
      else {
        val selves = selfCandidates(pt)
        if (selves.isEmpty || (isLexical(pt, y) && selves.forall { case (s, st) => beats(y.symbol, memberType(y), s, st) })) y else EmptyTree
      }

    // enclosing implicit vals/defs a search for `pt` can return, with their types. A field's getter is the candidate, and its info may not be
    // completed yet: the field's is. One without a result type is still being typed (no complete info) and is no candidate (scalac's isValid).
    private def selfCandidates(pt: Type): Seq[(Symbol, Type)] = selfSymbols.flatMap { s =>
      val tpe =
        if (!s.isTerm || !s.isImplicit || !(s.isMethod || !s.owner.isClass)) None
        else if (isComplete(s)) Some(s.info)
        else if (s.asTerm.isGetter && isComplete(s.asTerm.accessed)) Some(s.asTerm.accessed.info)
        else None
      tpe.filter { t =>
        val (tparams, res) = strip(t)
        res.substituteTypes(tparams, tparams.map(_ => WildcardType)) <:< pt
      }.map(s -> _)
    }

    private def isComplete(s: Symbol): Boolean = s.asInstanceOf[scala.reflect.internal.Symbols#Symbol].rawInfo.isComplete

    private def strip(t: Type): (List[Symbol], Type) = t match {
      case PolyType(tps, res)                             => val (more, r) = strip(res); (tps ++ more, r)
      case NullaryMethodType(res)                         => strip(res)
      case MethodType(ps, res) if ps.forall(_.isImplicit) => strip(res)
      case other                                          => (Nil, other)
    }

    // the type scalac ranks a candidate by: type parameters abstracted
    private def rank(t: Type): Type = { val (tparams, res) = strip(t); c.internal.existentialAbstraction(tparams, res) }

    private def memberType(y: Tree): Type = refOf(y) match {
      case Select(qual, _) if qual.tpe != null => y.symbol.typeSignatureIn(qual.tpe)
      case _                                   => y.symbol.info
    }

    private def refOf(y: Tree): Tree = y match {
      case Apply(fun, _)     => refOf(fun)
      case TypeApply(fun, _) => refOf(fun)
      case other             => other
    }

    // found in the lexical scope (a local, a member of an enclosing class, an import), not only in the implicit scope of `pt`
    private def isLexical(pt: Type, y: Tree): Boolean = refOf(y) match {
      case Ident(_)           => true
      case Select(This(_), _) => true
      case Select(qual, _)    => !implicitScopeModules(pt).contains(qual.symbol)
      case _                  => false
    }

    // the objects whose members are in the implicit scope of `pt`: companions of its parts, the objects they're nested in, and (2.13 without -Xsource:3) the package
    // objects of their packages
    private def implicitScopeModules(pt: Type): Set[Symbol] = {
      val acc = Set.newBuilder[Symbol]
      pt.foreach { t =>
        val ts = t.typeSymbol
        if (ts != NoSymbol) {
          if (ts.companion != NoSymbol) acc += ts.companion
          var o = ts.owner
          while (o != NoSymbol && o != c.mirror.RootClass) {
            if (o.isModuleClass) acc += o.asClass.module
            else if (o.isPackageClass) { val po = o.info.decl(termNames.PACKAGE); if (po != NoSymbol) acc += po }
            o = o.owner
          }
        }
      }
      acc.result()
    }

    private def beats(ySym: Symbol, yTpe: Type, sSym: Symbol, sTpe: Type): Boolean = {
      val (ry, rs)      = (rank(yTpe), rank(sTpe))
      val specificCount = (if (ry <:< rs) 1 else 0) - (if (rs <:< ry) 1 else 0)
      val subClassCount = (if (isProperSubClassOrObject(ySym.owner, sSym.owner)) 1 else 0) - (if (isProperSubClassOrObject(sSym.owner, ySym.owner)) 1 else 0)
      specificCount + subClassCount > 0
    }

    private def isProperSubClassOrObject(a: Symbol, b: Symbol): Boolean = {
      def linked(m: Symbol): Symbol = m.asClass.module.companion
      a != b && a != NoSymbol && b != NoSymbol && (
        (a.isClass && b.isClass && a.asClass.baseClasses.contains(b)) ||
          (a.isModuleClass && isProperSubClassOrObject(linked(a), b)) ||
          (b.isModuleClass && isProperSubClassOrObject(a, linked(b)))
      )
    }

    def isOwnerSelfRef(x: Tree): Boolean = {
      val s = x.symbol
      s != null && s != NoSymbol && s.isTerm && (enclosingSymbols.contains(s) || (s.asTerm.isGetter && enclosingSymbols.contains(s.asTerm.accessed)))
    }

    // `Json.auto.derived[T]` is an implicit Predef[T] macro. A search for Predef[T] returns it unexpanded, and it expands when our output is typechecked: a derivation of T inside the
    // derivation of T, forever (a scalac StackOverflowError in 0.7). It is also imported, so it shadows the built-in and companion Predefs (Predef[Int], ...).
    // So when the search picks `auto.derived` itself, search again with `derived` shadowed by a local non-implicit name (an implicit is only eligible if it's accessible by its simple
    // name), which finds the implicit the search would have found without the import. If that's `auto.derived` again (e.g. imported under another name), there's none: derive inline.
    // Hence importing Json.auto never turns a type into a definition and never changes a schema derived without it.
    // The ignoring search can't do this: it leaves `derived` out of the top-level candidates only, and nested searches expand it again (a StackOverflowError). The re-search
    // also shadows the enclosing definitions' names, so it doesn't find (and warn about) the self reference, and its result is checked like the ignoring search's.
    private def isAutoDerived(x: Tree): Boolean =
      x.symbol != null && x.symbol != NoSymbol && x.symbol.isMethod && x.symbol.fullName == "json.Json.auto.derived"

    private def inferPredef(pType: Type): Tree = infer(pType) match {
      case x if isAutoDerived(x) =>
        val shadows = (TermName("derived") +: selfNames).distinct.map(n => q"val $n = ()")
        c.typecheck(q"{ ..$shadows; _root_.scala.Predef.implicitly[$pType] }", silent = true) match {
          case Block(_, Apply(_, List(y))) if !isAutoDerived(y) => unlessSelfWins(pType, y)
          case _                                                => EmptyTree
        }
      case x => x
    }

    def get(tpe: Type): Option[SchemaType] = {

      val sType = appliedType(T.schemaC, tpe) // .dealias.widen
      val pType = appliedType(T.predefC, tpe) // .dealias.widen

      // search implicit scope for a schema
      //
      // this 2 level implicit search is done this way because Schema/Predef
      // are covariants which makes impossible use of implicit conversions like
      // ```
      // def schemaFromPredef[T](implicit p: Predef[T]): Schema[T] = p.schema
      // ```
      // if taken from schema - must be exposed as `ref`
      // if taken from predef - exposing as is
      def lookupPredef: ImplicitSchema = inferPredef(pType) match {
        case EmptyTree              => NotFound
        case x if isOwnerSelfRef(x) => NotFound
        case x                      => FromPredef(q"$x.schema")
      }

      // Schema is covariant, so a search for Schema[Base] also accepts an implicit Schema[Sub]. Only an implicit for exactly T counts (owner decision 2026-11-20): otherwise
      // Schema[Sub1] would stand for the whole sealed family of Base (Sub2 lost), or make a non-sealed trait "derivable".
      def isForExactly(x: Tree): Boolean =
        x.tpe != null && x.tpe.baseType(T.schemaC.typeSymbol).typeArgs.headOption.exists(_ =:= tpe)

      def lookupSchema: ImplicitSchema = {
        // schema
        infer(sType) match {
          case EmptyTree => lookupPredef
          // as on Scala 3: a declined self reference counts as no Schema, so a Predef of the type is still used
          case x if isOwnerSelfRef(x) => lookupPredef
          case x if !isForExactly(x)  => lookupPredef
          case x                      => FromSchema(x)
        }
      }

      lookupSchema match {
        case NotFound      => None
        case FromPredef(x) => Some(U.`-from-tree-`(tpe, x))
        // no compile-time name is right for every instantiation of an abstract type: a `def`/`ref` instance keeps its own name, anything else is inlined (#297; Scala 3: `FromImplicit` with no sig)
        case FromSchema(x) if mentionsAbstractType(tpe) => Some(U.`-from-tree-`(tpe, x))
        // a generic implicit named its definition for its own type parameters (`pkg.Tree[pkg.M.A]`): rename it and its refs to this type (#297; Scala 3: `FromImplicit.generic`)
        case FromSchema(x) if isGenericImplicit(x) =>
          val sig     = signature(tpe)
          val renamed = q"""$x match {
            case d: ${N.Schema}.`def`[_] => d.asInstanceOf[${N.Schema}.`def`[$tpe]].toDefinition[$tpe]($sig)
            case o                       => ${N.Schema}.`def`.adapt[$tpe](o, $sig)
          }"""
          Some(U.`-from-tree-`(tpe, renamed))
        case FromSchema(x) =>
          Some(U.`-from-tree-`(tpe, q"""${N.Schema}.`def`.adapt[$tpe]($x, ${signature(tpe)})"""))
      }
    }

    /** The found implicit is a method applied to type arguments (`jsonSchemaFoo[Bar](..)`). Scala 3: `Implicits.isGenericImplicit`. */
    private def isGenericImplicit(x: Tree): Boolean = x match {
      case Typed(e, _)     => isGenericImplicit(e)
      case Block(Nil, e)   => isGenericImplicit(e)
      case Apply(f, _)     => isGenericImplicit(f)
      case TypeApply(_, _) => true
      case _               => false
    }

    def getOrElse(tpe: Type, gen: => SchemaType): SchemaType = get(tpe) getOrElse gen
  }
  val Implicit = new Implicit
}
