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

    // under `AutoSearch.searching`: a `Json.auto.derived[X]` expanded by this search is a placeholder (a hole), see `asHole`
    private def infer(pt: Type): Tree = AutoSearch.searching {
      ImplicitSearch.inferIgnoring(c)(pt, selfSymbols) match {
        case Some(y) => unlessSelfWins(pt, y)
        case None    => c.inferImplicitValue(pt)
      }
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

    // ---- `import json.Json.auto._` (issue #542). `auto.derived[X]` is an `Exported[Predef[X]]`, reached through the bridge `Predef.importedPredef`. scalac expands a nested blackbox implicit
    // macro while the outer search is still choosing (spike 0(a): `importedPredef`'s implicit argument is expanded as soon as it is found), and under `AutoSearch.searching` the expansion is the
    // placeholder `Exported(AutoHole.hole[X])`, so a found tree looks like `listS[X](importedPredef[X](Exported(AutoHole.hole[X])))`. An unexpanded `auto.derived[X]` counts the same. Either is a
    // hole: a place where the instance takes a schema of `X` from auto, filled in from this expansion. The old re-search with `derived` shadowed is gone: an `Exported` never competes with a
    // `Predef`, so the import can't shadow the built-in and companion instances, and the placeholder can't recurse.

    private def symName(t: Tree): String = if (t.symbol == null || t.symbol == NoSymbol) "" else t.symbol.fullName

    private def isAutoDerivedRef(t: Tree): Boolean = symName(refOf(t)) == "json.Json.auto.derived"
    private def isHoleRef(t: Tree): Boolean        = symName(refOf(t)) == "com.github.andyglow.jsonschema.internal.AutoHole.hole"

    private lazy val exportedSym = typeOf[json.schema.Exported[_]].typeSymbol

    /** `t` is a hole: `(X, exported)`, `exported` if `t` is an `Exported[Predef[X]]` (else a `Predef[X]`). */
    private def asHole(t: Tree): Option[(Type, Boolean)] =
      if (t.tpe == null) None
      else {
        def predefArg(pt: Type): Option[Type] = pt.baseType(T.predefC.typeSymbol).typeArgs.headOption
        val exported                          = t.tpe.baseType(exportedSym).typeArgs.headOption
        val isHole                            = isAutoDerivedRef(t) || isHoleRef(t) || (exported.isDefined && containsHole(t))
        if (!isHole) None
        else
          exported match {
            case Some(pt) => predefArg(pt).map(_ -> true)
            case None     => predefArg(t.tpe).map(_ -> false)
          }
      }

    private def containsHole(t: Tree): Boolean = t.exists(x => x.tpe != null && (isAutoDerivedRef(x) || isHoleRef(x)))

    /** Any hole or unexpanded `auto.derived` left in a tree the macro emits (a bug if so). */
    def leftoverHole(t: Tree): Option[Tree] = t.find {
      case x if x.symbol != null && x.symbol != NoSymbol => isAutoDerivedRef(x) || isHoleRef(x)
      case Select(q, TermName("hole"))                   => q.toString.endsWith("AutoHole")
      case _                                             => false
    }

    private lazy val bridgeName = "json.schema.LowPriorityPredefs.importedPredef"

    /** The found `Predef[X]` is `importedPredef(<hole of X>)` (or the hole itself): auto is available for `X`, which is derived in place. */
    private def isRootHole(tpe: Type, x: Tree): Boolean = {
      def holeOfTpe(t: Tree) = asHole(t).exists(_._1 =:= tpe)
      holeOfTpe(x) || (x match {
        case Apply(fun, List(arg)) if symName(refOf(fun)) == bridgeName => holeOfTpe(arg)
        case _                                                          => false
      })
    }

    // core's own Predef constructors for collections (`listS` etc.): with a hole inside, the native node, which renders the same (design S2-R8) and keeps today's output by construction
    private lazy val coreConstructors = Set("arrayS", "setS", "listS", "vectorS", "dictS").map("json.schema.Predef." + _)

    private def isCoreConstructorWithHole(x: Tree): Boolean = coreConstructors.contains(symName(refOf(x))) && containsHole(x)

    /** The found `x` with every hole replaced by an `Ident` of a fresh name, untyped, with each hole's schema of `X` resolved in this expansion. The shape is checked with a stub for each hole: if it
      * doesn't typecheck, it is a compile error (never a derivation of `tpe` from its structure, which would bring back #482).
      */
    private def withHoles(tpe: Type, x: Tree)(implicit ctx: ResolutionContext): (Tree, List[U.Hole]) = {
      var holes   = List.empty[U.Hole]
      val punched = new Transformer {
        override def transform(t: Tree): Tree = asHole(t) match {
          case Some((xTpe, exported)) =>
            val name = TermName(c.freshName("jsonSchemaAuto"))
            holes = holes :+ U.Hole(name, xTpe, exported, null)
            Ident(name)
          case None => super.transform(t)
        }
      }.transform(x)
      val untyped = c.untypecheck(staticQualifiers(punched))
      val stubs   = holes map { h =>
        val holeTpe = if (h.exported) tq"_root_.json.schema.Exported[_root_.json.schema.Predef[${h.tpe}]]" else tq"_root_.json.schema.Predef[${h.tpe}]"
        q"val ${h.name}: $holeTpe = null.asInstanceOf[$holeTpe]"
      }
      val problem = try {
        val checked = c.typecheck(q"{ ..$stubs; ${untyped.duplicate} }")
        leftoverHole(checked).map(h => s"a placeholder is left: ${showCode(h)}")
      } catch { case e: scala.reflect.macros.TypecheckException => Some(e.msg) }
      problem foreach { why =>
        c.abort(
          c.enclosingPosition,
          s"schema for ${showType(tpe)} is not supported${pathOf(ctx.stack)}: the implicit found for it, `${showCode(x)}`, takes a schema of " +
            holes.map(h => showType(h.tpe)).distinct.mkString(", ") + s" from `import json.Json.auto._`, and the derivation can't fill it in ($why). Provide a json.schema.Predef or json.Schema for " +
            showType(tpe)
        )
      }
      (untyped, holes map { h => h.copy(schema = resolve(h.tpe, ctx :+ tpe)) })
    }

    /** A package or a static object referred to as `pkg.this` (how a search refers to implicit-scope members) becomes its full path: after `untypecheck`, `schema.this.Predef` would be looked up in
      * the call site's enclosing classes.
      */
    private def staticQualifiers(t: Tree): Tree = new Transformer {
      // by the owners' own (encoded) names: `fullName` is decoded (`lazy-ref`)
      private def fullPath(sym: Symbol): Tree = {
        val path = Iterator.iterate(sym)(_.owner).takeWhile(s => s != NoSymbol && s != c.mirror.RootClass).toList.reverse
        path.foldLeft[Tree](Ident(termNames.ROOTPKG)) { (q, s) => Select(q, s.name.toTermName) }
      }
      // the empty package has no path: left as it is
      private def hasPath(sym: Symbol): Boolean = !Iterator.iterate(sym)(_.owner).takeWhile(_ != NoSymbol).exists(_.isEmptyPackageClass)
      override def transform(t: Tree): Tree     = t match {
        case th @ This(_) if th.symbol != null && th.symbol.isPackageClass && hasPath(th.symbol)                      => fullPath(th.symbol)
        case th @ This(_) if th.symbol != null && th.symbol.isModuleClass && th.symbol.isStatic && hasPath(th.symbol) => fullPath(th.symbol)
        case other                                                                                                    => super.transform(other)
      }
    }.transform(t)

    sealed trait Lookup
    object Lookup {

      /** an implicit instance */
      case class Found(st: SchemaType) extends Lookup

      /** none, but `import json.Json.auto._` makes the type derivable here: derive it in place, in auto mode */
      case object AutoAvailable extends Lookup

      case object Missing extends Lookup
    }

    def lookup(tpe: Type)(implicit ctx: ResolutionContext): Lookup = {
      import Lookup._

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
      def lookupPredef: ImplicitSchema = infer(pType) match {
        case EmptyTree              => NotFound
        case x if isOwnerSelfRef(x) => NotFound
        case x                      => FromPredef(x)
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

      // `wrap(x)`: the emitted tree for the found `x`; with holes, `x` is untyped and holds an `Ident` per hole
      def fromTree(x: Tree)(wrap: Tree => Tree): SchemaType =
        if (!containsHole(x)) U.`-from-tree-`(tpe, wrap(x))
        else {
          val (body, holes) = withHoles(tpe, x)
          U.`-from-tree-`(tpe, wrap(body), holes = holes)
        }

      lookupSchema match {
        case NotFound                                      => Missing
        case FromPredef(x) if isRootHole(tpe, x)           => AutoAvailable
        case FromPredef(x) if isCoreConstructorWithHole(x) => AutoAvailable
        case FromPredef(x)                                 => Found(fromTree(x)(y => q"$y.schema"))
        // no compile-time name is right for every instantiation of an abstract type: a `def`/`ref` instance keeps its own name, anything else is inlined (#297; Scala 3: `FromImplicit` with no sig)
        case FromSchema(x) if mentionsAbstractType(tpe) => Found(fromTree(x)(identity))
        // a generic implicit named its definition for its own type parameters (`pkg.Tree[pkg.M.A]`): rename it and its refs to this type (#297; Scala 3: `FromImplicit.generic`).
        // Through `adaptOrLazy`, as below: the renamed definition, or a `lazy-ref` in a recursion cycle between instances
        case FromSchema(x) if isGenericImplicit(x) =>
          val sig = signature(tpe)
          Found(fromTree(x) { y =>
            q"""${N.Schema}.`def`.adaptOrLazy[$tpe]($y match {
              case d: ${N.Schema}.`def`[_] => d.asInstanceOf[${N.Schema}.`def`[$tpe]].toDefinition[$tpe]($sig)
              case o                       => o
            }, $sig)"""
          })
        // a definition + `$ref`; a `lazy-ref` if the instance isn't available yet (a recursion cycle between instances, a val read before it is initialized: issue #542)
        case FromSchema(x) =>
          val sig = signature(tpe)
          Found(fromTree(x)(y => q"""${N.Schema}.`def`.adaptOrLazy[$tpe]($y, $sig)"""))
      }
    }

    /** An implicit instance of `tpe`; `None` also when only auto could make one (the caller derives it). */
    def get(tpe: Type)(implicit ctx: ResolutionContext): Option[SchemaType] = lookup(tpe) match {
      case Lookup.Found(st) => Some(st)
      case _                => None
    }

    /** The found implicit is a method applied to type arguments (`jsonSchemaFoo[Bar](..)`). Scala 3: `Implicits.isGenericImplicit`. */
    private def isGenericImplicit(x: Tree): Boolean = x match {
      case Typed(e, _)     => isGenericImplicit(e)
      case Block(Nil, e)   => isGenericImplicit(e)
      case Apply(f, _)     => isGenericImplicit(f)
      case TypeApply(_, _) => true
      case _               => false
    }
  }
  val Implicit = new Implicit
}
