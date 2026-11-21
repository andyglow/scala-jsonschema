package com.github.andyglow.jsonschema.derivation

/** Recursive types (Scala 2's `URecursiveTypes`).
  *
  * During construction, [[Resolve.resolve]] meets a type that is already on the stack of types being derived: it records the type here and returns a [[Nodes.RefTo]]. After construction,
  * [[substituteRecursive]] wraps every node of a recorded type in a [[Nodes.Def]] (the root too, when it is the recursive type).
  *
  * The `$ref` takes the name of the *stack entry* it points back to, which is the type of the node that becomes the definition. So a ref and its definition always agree, also when the recursion goes
  * through a type alias (TRIAGE-B: Scala 2 named the ref after the alias and the definition after the class, leaving a dangling `$ref`).
  */
trait Recursion { this: Derivation =>
  import q.reflect.*

  private var recursive: List[TypeRepr] = Nil

  /** `tpe` was met again while being derived; `onStack` is the stack entry it matched. */
  def onCycle(tpe: TypeRepr, onStack: TypeRepr): Node = {
    if (!recursive.exists(_ =:= onStack)) recursive = recursive :+ onStack
    RefTo(tpe, signature(onStack, dealiased = true))
  }

  def isRecursive(tpe: TypeRepr): Boolean = recursive.exists(_ =:= tpe)

  def substituteRecursive(n: Node): Node =
    if (recursive.isEmpty) n
    else
      transform(n) {
        case d: Def                  => d
        case r: RefTo                => r
        case x if isRecursive(x.tpe) => Def(x.tpe, signature(x.tpe, dealiased = true), x)
      }
}
