package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import json.Schema

/** The IR (intermediate representation) of the derivation: a tree of [[Node]]s, one per Scala type met.
  *
  * The derivation runs in three separate passes:
  *   1. construction ([[Resolve.resolve]] and the families it dispatches to) builds the tree. It records *semantic facts* (optionality, sum-family membership, member names, product kinds, value-class
  *      wrappers, ...) even where today's output doesn't use them (profiledesign's requirements, `notes/scratch/profiledesign/ir-facts.md`);
  *   2. IR -> IR passes: the recursion pass ([[Recursion]]) and, per family, any normalization (e.g. flattening nested sealed families);
  *   3. emission ([[Emit]]): every node turns itself into an `Expr[Schema[?]]` (`emitRaw`), and the generic emitter applies the node's [[Extra]].
  *
  * The node classes are NOT sealed: each type family defines its own nodes in its own file (Products.scala, Sums.scala, Collections.scala, ...), so the family streams don't edit a shared file.
  * Generic passes only use the [[Node]] interface (`children` / `mapChildren`). See `notes/reports/skeleton-ir.md`.
  */
trait Nodes { this: Derivation =>
  import q.reflect.*

  /** Decorations shared by every node (Scala 2's `SchemaType.Extra`). Emitted in this order: validations, title, description, discriminationKey. */
  final case class Extra(
    validations: List[Validation] = Nil,
    title: Option[String] = None,
    description: Option[String] = None,
    discriminationKey: Option[String] = None,
    deprecated: Boolean = false
  ) {
    def orElse(o: Extra): Extra = Extra(
      validations = if (validations.nonEmpty) validations else o.validations,
      title = title.orElse(o.title),
      description = description.orElse(o.description),
      discriminationKey = discriminationKey.orElse(o.discriminationKey),
      deprecated = deprecated || o.deprecated
    )
  }

  /** A validation the macro attaches to a node (e.g. `patternProperties` for a dictionary key). Applied with `withValidation`. */
  trait Validation {
    def apply(schema: Expr[Schema[?]]): Expr[Schema[?]]
  }

  /** An IR node.
    *
    * `tpe` is the Scala type the node stands for, dealiased at the top level ([[Resolve.resolve]] dealiases every type it meets; type arguments may still be aliases). Definition and `$ref` names are
    * computed from it with `signature(tpe, dealiased = true)`.
    */
  trait Node {
    def tpe: TypeRepr
    def extra: Extra
    def withExtra(x: Extra): Node

    /** Direct sub-nodes, in a stable order. */
    def children: List[Node]

    /** The same node with every direct sub-node replaced by `f(sub-node)`. */
    def mapChildren(f: Node => Node): Node

    /** The schema expression for this node, without [[extra]] (the emitter applies it). Use [[Emit.emit]] for children. */
    def emitRaw: Expr[Schema[?]]
  }

  /** Where an implicitly found schema came from. */
  enum ImplicitSource {

    /** A user-provided implicit `Schema[T]` (incl. a `derives json.Schema` given): rendered as a definition + `$ref`. */
    case UserSchema

    /** An implicit `Predef[T]` (all built-ins): inlined. */
    case Predef
  }

  /** A schema found by implicit search ([[Implicits]]).
    *
    * @param sig
    *   the definition name for [[ImplicitSource.UserSchema]]; `None` when `tpe` mentions an abstract type (a type parameter or abstract type member, e.g. the `Schema[T]` that a generic `derives`
    *   receives): no name known at compile time is right for every instantiation. Then the decision is made at RUNTIME (lead decision 2026-11-20, a deliberate Scala 3 deviation; Scala 2 names the
    *   definition after the type parameter, `pkg.Owner.T`, and its analogous case overflows the stack): an instance that is already a definition or a reference keeps its own name (`$ref`), anything
    *   else is inlined.
    * @param generic
    *   the implicit is generic (a given/def with type parameters, applied to this type's arguments: the given of a generic `derives`). A definition it returns was named inside it, for its type
    *   parameters (`pkg.GRec[pkg.GRec.T]` for a recursive generic `derives`), the same name for every instantiation. That name is provisional: the definition and its `$ref`s are renamed to `sig`, the
    *   concrete type's name, at runtime with `toDefinition` (lead decision on reviewer3 round 2, BACKLOG #9).
    */
  final case class FromImplicit(tpe: TypeRepr, expr: Expr[Schema[?]], source: ImplicitSource, sig: Option[String], generic: Boolean = false, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = Nil
    def mapChildren(f: Node => Node): Node = this
    def emitRaw: Expr[Schema[?]]           = (source, sig) match {
      case (ImplicitSource.Predef, _)              => expr
      case (ImplicitSource.UserSchema, None)       => expr // a `def`/`ref` instance stays one, anything else is inlined as is
      case (ImplicitSource.UserSchema, Some(name)) =>
        tpe.asType match {
          case '[t] =>
            val se = Expr(name)
            if (generic) '{
              $expr match {
                case d: Schema.`def`[?] => d.asInstanceOf[Schema.`def`[t]].toDefinition[t]($se) // renames the definition and its $refs
                case other              => Schema.`def`.adapt[t](other, $se)
              }
            }
            else '{ Schema.`def`.adapt[t]($expr, $se) }
        }
    }
  }

  /** A named definition (`$ref` target). Emitted with `def.adapt`, like a user implicit `Schema`: a value class is defined by its underlying schema (Scala 2 parity). */
  final case class Def(tpe: TypeRepr, sig: String, schema: Node, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = List(schema)
    def mapChildren(f: Node => Node): Node = copy(schema = f(schema))
    def emitRaw: Expr[Schema[?]]           = tpe.asType match {
      case '[t] =>
        val se    = Expr(sig)
        val inner = emit(schema)
        '{ Schema.`def`.adapt[t]($inner, $se) }
    }
  }

  /** A reference to a definition (`$ref`): produced for recursive types. (Not called `Ref` to avoid a clash with `quotes.reflect.Ref`.) */
  final case class RefTo(tpe: TypeRepr, sig: String, extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = Nil
    def mapChildren(f: Node => Node): Node = this
    def emitRaw: Expr[Schema[?]]           = tpe.asType match {
      case '[t] =>
        val se = Expr(sig)
        '{ Schema.`ref`[t]($se) }
    }
  }

  /** Post-order transformation: children first, then `pf` on the node (Scala 2's `transformSchema`). */
  def transform(n: Node)(pf: PartialFunction[Node, Node]): Node = {
    val out = n.mapChildren(transform(_)(pf))
    if (pf.isDefinedAt(out)) pf(out) else out
  }
}
