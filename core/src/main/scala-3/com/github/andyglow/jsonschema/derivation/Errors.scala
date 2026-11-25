package com.github.andyglow.jsonschema.derivation

/** Compile errors: the same texts as the Scala 2 macro (`macros/src/main/scala-2/.../UCommons.scala`), pinned by `ErrorMessagesSpec`:
  * `schema for <T> is not supported[ (in <A> :: <B>)][: <reason>]. <hint>`. A reason that carries its own advice replaces the hint.
  */
trait Errors { this: Derivation =>
  import q.reflect.*

  val ProvideHint = "Provide a json.schema.Predef or json.Schema for it"

  val NullReason = "Null has no schema of its own. For a JSON null, provide an implicit json.schema.Predef[Null], e.g. json.schema.Predef(json.Schema.`null`)"

  val WildcardReason = s"a wildcard type argument (?) has no schema. Use a concrete type, or provide a json.schema.Predef or json.Schema for it"

  /** A type in messages, as Scala 2's [[showType]] prints it: `show` (fully qualified), a singleton as `pkg.Obj.type` (also as a type argument), a wildcard as `?`. */
  def showType(t: TypeRepr): String = t match {
    case _: TermRef            => s"${t.show}.type"
    case AppliedType(tc, args) => showType(tc) + args.map(showType).mkString("[", ", ", "]")
    case TypeBounds(lo, hi)    =>
      if (hi =:= TypeRepr.of[Any] && lo =:= TypeRepr.of[Nothing]) "?"
      else if (lo =:= TypeRepr.of[Nothing]) s"? <: ${showType(hi)}"
      else if (hi =:= TypeRepr.of[Any]) s"? >: ${showType(lo)}"
      else s"? >: ${showType(lo)} <: ${showType(hi)}"
    case _ => t.show
  }

  /** ` (in A :: B)`, outermost first; empty at the root. */
  def pathOf(ctx: Ctx): String = if (ctx.isRoot) "" else ctx.stack.map(showType).mkString(" (in ", " :: ", ")")

  /** The "not supported" error: `reason` (which then carries its own advice) or the default hint; `suffix` goes last (refined's failed implicit). */
  def notSupported(tpe: TypeRepr, ctx: Ctx, reason: String = "", suffix: String = ""): Nothing =
    report.errorAndAbort(s"schema for ${showType(tpe)} is not supported${pathOf(ctx)}" + (if (reason.isEmpty) s". $ProvideHint" else s": $reason") + suffix)

  // ---- semi-auto (issue #542): the error (Flag.SemiAuto, phase 2) and the phase-1 warning, with the same fixes

  /** The stable prefix of the phase-1 warning: `-Wconf:msg=json-schema-semi-auto:s` silences it. */
  val SemiAutoPrefix = "[json-schema-semi-auto]"

  private def semiAutoFixes(tpe: TypeRepr): String = {
    val short = shortType(tpe)
    val name  = valName(tpe)
    s"Define one: `implicit val $name: json.Schema[$short] = json.Json.schema[$short]` (a definition + $$ref; for a generic type " +
      "`implicit def boxSchema[T](implicit p: json.schema.Predef[T]): json.Schema[Box[T]] = json.Json.schema[Box[T]]`),\n" +
      "or derive nested types automatically: `import json.Json.auto._` (Scala 3: `import json.Json.auto.given`)"
  }

  /** Why the search for `tpe`'s `Predef` failed, when the compiler says more than "none found" (`NonEmptyList[P]`: the `Predef[P]` its instance needs). */
  private def semiAutoReason(tpe: TypeRepr): String =
    implicitFailures(tpe).map(e => s"\n  an implicit for it failed: $e").mkString + missReasons(tpe).map(e => s"\n  $e").mkString

  def semiAutoError(tpe: TypeRepr, ctx: Ctx): Nothing =
    report.errorAndAbort(s"schema for ${showType(tpe)} is not supported${pathOf(ctx)}: no implicit json.Schema or json.schema.Predef for it.\n${semiAutoFixes(tpe)}${semiAutoReason(tpe)}")

  def semiAutoWarning(tpe: TypeRepr, ctx: Ctx): String =
    s"$SemiAutoPrefix schema for ${showType(tpe)} is derived implicitly${pathOf(ctx)}; this becomes an error in a future release. ${semiAutoFixes(tpe)}"

  /** `tpe` as the user writes it: simple names (`Address`, `NonEmptyList[P]`). */
  def shortType(t: TypeRepr): String = t.dealias match {
    case AppliedType(tc, args) => shortType(tc) + args.map(shortType).mkString("[", ", ", "]")
    case t: TermRef            => s"${t.termSymbol.name}.type"
    case t                     => t.typeSymbol.name
  }

  /** `addressSchema` for `Address`. */
  private def valName(t: TypeRepr): String = {
    val n = t.dealias.typeSymbol.name.filter(_.isLetterOrDigit)
    (if (n.isEmpty) "x" else n.head.toLower.toString + n.tail) + "Schema"
  }

  /** The context without its innermost entry (the family a member error names). */
  def outer(ctx: Ctx): Ctx = Ctx(ctx.stack.dropRight(1))

  /** The reason for a singleton type `X.type` of an object: use its sealed family, if it has one. */
  def singletonReason(tpe: TypeRepr): Option[String] = tpe match {
    case t: TermRef if t.termSymbol.flags.is(Flags.Module) =>
      val cls = t.termSymbol.moduleClass
      t.baseClasses.filterNot(_ == cls).find(b => b.flags.is(Flags.Sealed)) match {
        case Some(b) =>
          Some(
            s"a singleton type (the type of one object) has no schema of its own. Use the type of its sealed family ${b.typeRef.show} instead, or provide a json.schema.Predef or json.Schema for it"
          )
        case None =>
          Some("a singleton type (the type of one object) has no schema of its own; objects are derived only as members of a sealed family. Provide a json.schema.Predef or json.Schema for it")
      }
    case _ => None
  }

  /** A wildcard reached as a type: a bare `TypeBounds` (the argument of `Option[?]`), or a type parameter seen through a wildcard-applied prefix (`Box[?]#A`, a field of `Box[?]`). */
  def isWildcard(tpe: TypeRepr): Boolean = tpe match {
    case _: TypeBounds => true
    case t: TypeRef    =>
      t.typeSymbol.isTypeParam && (t.qualifier match {
        case AppliedType(_, args) => args.exists { case _: TypeBounds => true; case _ => false }
        case _                    => false
      })
    case _ => false
  }
}
