package com.github.andyglow.jsonschema

import scala.reflect.internal.util.NoSourceFile

private[jsonschema] trait UImplicits { this: UContext with UCommons with USignatures =>
  import c.universe._

  class Implicit {
    final val LF = '\u000A'
    final val CR = '\u000D'

    sealed trait ImplicitSchema
    case class FromPredef(x: Tree) extends ImplicitSchema
    case class FromSchema(x: Tree) extends ImplicitSchema
    case object NotFound           extends ImplicitSchema

    // FIXME: this method is pretty ugly
    //   it is trying to handle situation like this
    //   {{{
    //     case class CC(a: String)
    //     implicit val aSchema: Schema[CC] = Json.schema[CC]
    //   }}}
    //   in this case `inferImplicitValue` returns a self-reference
    //   which turns out to be resolved as `null` eventually
    //
    //   So this method take a line of a source code where our def-macro is used and
    //   see if it is assignment and it assigns to variable returned by `inferImplicitValue`
    //
    def isSelfRef(x: Tree): Boolean = x match {
      case x @ Select(This(_), TermName(field)) =>
        val pos = x.pos
        pos.source match {
          case NoSourceFile => false
          case src          =>
            val end   = src.lineToOffset(src.offsetToLine(pos.point))
            var start = end - 2

            while ({
              !src.isLineBreak(start) &&
              !src.isEndOfLine(start)
            }) start = start - 1

            val str = new String(src.content, start, pos.point - start)
            if (str.contains(field) && str.contains("=") && str.contains("implicit")) true
            else false
        }
      case _ => false
    }

    // the symbols whose right-hand side contains this expansion: the val, lazy val or def being defined and its owners
    private lazy val enclosingSymbols: Set[Symbol] = {
      def loop(s: Symbol, acc: Set[Symbol]): Set[Symbol] = if (s == NoSymbol || s.isPackageClass) acc else loop(s.owner, acc + s)
      loop(c.internal.enclosingOwner, Set.empty)
    }

    // Owner-based self-reference check, as on Scala 3: the found implicit is the val/def whose right-hand side we're expanding in (`implicit val p: Predef[X] = Predef(Json.schema[X])`).
    // It is still null when the derived code runs (a def recurses forever). The right-hand side of a field is owned by the field, the search finds its getter.
    // Used for Predef and, next to the source-text check above, for Schema (that check only sees two lines; its false positives are TRIAGE-A, kept for now).
    def isOwnerSelfRef(x: Tree): Boolean = {
      val s = x.symbol
      s != null && s != NoSymbol && s.isTerm && (enclosingSymbols.contains(s) || (s.asTerm.isGetter && enclosingSymbols.contains(s.asTerm.accessed)))
    }

    // `Json.auto.derived[T]` is an implicit Predef[T] macro. A search for Predef[T] returns it unexpanded, and it expands when our output is typechecked: a derivation of T inside the
    // derivation of T, forever (a scalac StackOverflowError in 0.7). It is also imported, so it shadows the built-in and companion Predefs (Predef[Int], ...).
    // So when the search picks `auto.derived` itself, search again with `derived` shadowed by a local non-implicit name (an implicit is only eligible if it's accessible by its simple
    // name), which finds the implicit the search would have found without the import. If that's `auto.derived` again (e.g. imported under another name), there's none: derive inline.
    // Hence importing Json.auto never turns a type into a definition and never changes a schema derived without it.
    private def isAutoDerived(x: Tree): Boolean =
      x.symbol != null && x.symbol != NoSymbol && x.symbol.isMethod && x.symbol.fullName == "json.Json.auto.derived"

    private def inferPredef(pType: Type): Tree = c.inferImplicitValue(pType) match {
      case x if isAutoDerived(x) =>
        c.typecheck(q"{ val derived = (); _root_.scala.Predef.implicitly[$pType] }", silent = true) match {
          case Block(_, Apply(_, List(y))) if !isAutoDerived(y) => y
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
        c.inferImplicitValue(sType) match {
          case EmptyTree => lookupPredef
          // 0.7's source-text check skips the Predef search (kept: TRIAGE-A)
          case x if isSelfRef(x) => NotFound
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
