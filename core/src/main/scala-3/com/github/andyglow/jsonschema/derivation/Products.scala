package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.json.{ToValue, Value}
import json.Schema

/** Products: case classes and tuples -> `object` (Scala 2's `UProductTypes`).
  *
  * Fields are the first parameter list, in declaration order. Field types have the case class's type arguments substituted (`memberType`) and are dealiased at the top level (Scala 2 quirk 2). An
  * `Option[X]` field becomes an [[Opt]] around X, emitted as `option(X)` and not required (a JsonSchemaProfile shapes it at render time). A default makes the field not required; its value is computed
  * at RUNTIME by calling the companion's default getter (`$lessinit$greater$default$N`) and converted with `ToValue`, so `= None` emits no default when a `ToValue[Option[X]]` is in scope. Without
  * one, only a `None` / `Option.empty` default compiles ([[optionDefault]], Scala 2 parity).
  *
  * Field decorations: descriptions from `@description`, scaladoc `@param` and `Json.objectSchema` ([[fieldDescriptions]]), `@readOnly` / `@writeOnly` ([[fieldRWMode]]).
  *
  * Owner: skeleton + stream 1.
  */
trait Products { this: Derivation =>
  import q.reflect.*

  /** What kind of product the object stands for (profiledesign fact 4). */
  enum ProductKind {
    case CaseClass
    case Tuple(arity: Int)
  }

  enum RWMode {
    case ReadWrite, ReadOnly, WriteOnly
  }

  /** A field of an [[Obj]].
    * @param name
    *   the (decoded) parameter name
    * @param tpe
    *   the declared type, type arguments substituted, dealiased at the top level
    * @param schema
    *   the field type's node; an [[Opt]] for `Option` fields
    * @param optional
    *   the field is an `Option` (kept apart from `required` and `hasDefault`: profiledesign fact 1)
    * @param hasDefault
    *   the parameter has a default
    * @param default
    *   the default value, computed at runtime (`None` at runtime for `= None`)
    * @param annotations
    *   the annotations of the constructor parameter and of the field
    */
  final case class Field(
    name: String,
    tpe: TypeRepr,
    schema: Node,
    optional: Boolean,
    hasDefault: Boolean,
    default: Option[Expr[Option[Value]]],
    description: Option[String],
    rwMode: RWMode,
    annotations: List[Term]
  ) {
    def required: Boolean = !optional && !hasDefault
  }

  final case class Obj(tpe: TypeRepr, kind: ProductKind, fields: List[Field], extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = fields.map(_.schema)
    def mapChildren(f: Node => Node): Node = copy(fields = fields.map(fl => fl.copy(schema = f(fl.schema))))
    def emitRaw: Expr[Schema[?]]           = tpe.asType match {
      case '[t] =>
        val fs = Expr.ofList(fields.map(emitField))
        '{ Schema.`object`.fromList[t]($fs) }
    }
  }

  def emitField(f: Field): Expr[Schema.`object`.Field[?]] = f.schema.tpe.asType match {
    case '[ft] =>
      val schema = emit(f.schema).asExprOf[Schema[ft]]
      val name   = Expr(f.name)
      val req    = Expr(f.required)
      val base   = f.default match {
        case Some(d) => '{ Schema.`object`.Field.fromJson[ft]($name, $schema, $req, $d) }
        case None    => '{ Schema.`object`.Field[ft]($name, $schema, $req) }
      }
      val withDescr = f.description.fold(base) { d =>
        val de = Expr(d)
        '{ $base.withDescription(Some($de)) }
      }
      val withRW = f.rwMode match {
        case RWMode.ReadWrite => withDescr
        case RWMode.ReadOnly  => '{ $withDescr.withRWMode(Schema.`object`.Field.RWMode.ReadOnly) }
        case RWMode.WriteOnly => '{ $withDescr.withRWMode(Schema.`object`.Field.RWMode.WriteOnly) }
      }
      // the Scala default may have no JSON form (`= None`, no ToValue in scope): the fact is kept for the profiles
      if (f.hasDefault) '{ $withRW.withHasDefault(true) } else withRW
  }

  // ---- construction

  /** A constructor parameter of a product type. `declared` has the owner's type arguments substituted; `tpe` is `declared` dealiased at the top level (Scala 2 dealiases field types). */
  final case class Param(name: String, sym: Symbol, declared: TypeRepr, index: Int, annotations: List[Term]) {
    def tpe: TypeRepr = declared.dealias
  }

  /** The first parameter list of `tpe`'s primary constructor, with field types substituted and dealiased. */
  def params(tpe: TypeRepr): List[Param] = {
    val cls   = classSymbol(tpe)
    val owner = tpe.dealias
    val first = cls.primaryConstructor.paramSymss.filterNot(_.headOption.exists(_.isTypeParam)).headOption getOrElse Nil
    first.zipWithIndex map { (p, i) =>
      val field = cls.fieldMember(p.name)
      val ft    = if (field.isNoSymbol) owner.memberType(p) else owner.memberType(field)
      val ann   = annotationsOf(p) ++ annotationsOf(field)
      Param(p.name, p, ft, i, ann)
    }
  }

  def deriveProduct(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    Option.when(isCaseClass(tpe))(product(tpe, ctx))

  def product(tpe: TypeRepr, ctx: Ctx): Obj = {
    val cls = classSymbol(tpe)
    if (cls.flags.is(Flags.Abstract)) report.errorAndAbort(s"No apply function found for ${fullName(cls)}")
    val ps     = params(tpe)
    val descrs = fieldDescriptions(tpe, ps, ctx)
    val fields = ps map { p =>
      val inner    = optionElement(p.tpe)
      val typeNode = inner match {
        case Some(e) => Opt(optionOf(e), resolve(e, ctx.descend(tpe))) // `Option[e]`, also for a `Some[e]` field
        case None    => resolve(p.tpe, ctx.descend(tpe))
      }
      // `@title` on a field goes on the field's schema (`object.Field` has no title), over its type's
      val node       = fieldTitle(p).fold(typeNode)(t => typeNode.withExtra(typeNode.extra.copy(title = Some(t))))
      val hasDefault = p.sym.flags.is(Flags.HasDefault)
      Field(
        name = p.name,
        tpe = p.tpe,
        schema = node,
        optional = inner.isDefined,
        hasDefault = hasDefault,
        default = Option.when(hasDefault)(defaultValue(tpe, p, inner)),
        description = descrs.get(p.name),
        rwMode = fieldRWMode(p),
        annotations = p.annotations
      )
    }
    val kind = if (defn.isTupleClass(cls)) ProductKind.Tuple(fields.size) else ProductKind.CaseClass
    Obj(tpe, kind, fields)
  }

  /** `Some(X)` if `tpe` is an `Option[X]` (Scala 2: `<:< Option[_]`). */
  def optionElement(tpe: TypeRepr): Option[TypeRepr] =
    if (tpe <:< TypeRepr.of[Option[Any]]) tpe.baseType(TypeRepr.of[Option[Any]].typeSymbol).typeArgs.headOption
    else None

  /** A field's `@title` (on the parameter or its field). */
  def fieldTitle(p: Param): Option[String] =
    p.annotations.filter(_.tpe <:< TypeRepr.of[json.schema.title]).map(stringArg(_, "text", 0)).collectFirst { case Arg.Given(t) => t }

  /** Field descriptions by field name (Scala 2's `UFieldDecorations`): `@description` on the parameter or its field, then the type's scaladoc `@param`s, then the `Json.objectSchema` decorations (root
    * only); a later source wins (inventory F7: objectSchema config > scaladoc `@param` > `@description`).
    *
    * Checks (s2fix): an objectSchema decoration must name a field (compile error); a scaladoc `@param` that names no field is a warning, on the root and nested alike (TRIAGE-B).
    */
  def fieldDescriptions(tpe: TypeRepr, ps: List[Param], ctx: Ctx): Map[String, String] = {
    val names           = ps.map(_.name)
    val fromAnnotations = ps.flatMap { p =>
      p.annotations.filter(_.tpe <:< TypeRepr.of[json.schema.description]).map(stringArg(_, "text", 0)).collectFirst { case Arg.Given(d) => p.name -> d }
    }.toMap
    val cls          = classSymbol(tpe)
    val fromScaladoc = scaladocOf(cls).fold(Map.empty[String, String])(docParams)
    val stale        = fromScaladoc.keys.filterNot(names.contains).toList.sorted
    // only for types compiled in this run: a library's stale scaladoc (read from TASTy with -Yread-docs) is not the user's to fix, and Scala 2 can't see it
    if (stale.nonEmpty && cls.isDefinedInCurrentRun) {
      val t = signature(tpe, dealiased = true)
      report.warning(
        s"scaladoc of $t has @param for unknown field(s): ${stale.mkString(", ")}. $t fields are: ${names.mkString(", ")}",
        cls.pos getOrElse Position.ofMacroExpansion
      )
    }
    val fromSpec = if (ctx.isRoot) objectSchemaDecorations else Nil
    fromSpec foreach { (k, _) =>
      if (!names.contains(k)) report.errorAndAbort(s"unknown field: $k. ${signature(tpe, dealiased = true)} fields are: ${names.mkString(", ")}")
    }
    fromAnnotations ++ fromScaladoc ++ fromSpec
  }

  /** `@readOnly` / `@writeOnly` on the parameter or its field; both together = neither (inventory F11). */
  def fieldRWMode(p: Param): RWMode =
    (hasAnnotation[json.schema.readOnly](p.annotations), hasAnnotation[json.schema.writeOnly](p.annotations)) match {
      case (true, false) => RWMode.ReadOnly
      case (false, true) => RWMode.WriteOnly
      case _             => RWMode.ReadWrite
    }

  // ---- defaults

  /** The runtime default of `p`: a call of the companion's default getter, converted with `ToValue` (inventory F3). Without a `ToValue` it's a compile error, as in Scala 2. */
  def defaultValue(owner: TypeRepr, p: Param, optionOf: Option[TypeRepr]): Expr[Option[Value]] = {
    val getter = defaultGetter(owner, p)
    val call   = defaultGetterCall(owner, getter)
    optionOf match {
      case Some(e) => optionDefault(owner, p, getter, call, e)
      case None    =>
        p.tpe.asType match {
          case '[f] =>
            val toV = Expr.summon[ToValue[f]] getOrElse noToValue(owner, p, getter)
            val v   = call.asExprOf[f]
            '{ Some($toV($v)) }
        }
    }
  }

  /** The default of an `Option[E]` field (inventory F4): `None` gives no default (the field is just not required); anything else needs a `ToValue[Option[E]]` (core has none, the bridges bring one),
    * like Scala 2.
    *
    * With a `ToValue[Option[E]]`, `None` is told apart at runtime. Without one, it is decided at compile time (skeleton open point D2, see [[defaultIsNone]]): `None` / `Option.empty` gives no
    * default, anything else (`Some(..)`, `Defs.opt`) is a compile error. Scala 2 evaluates the default with `c.eval`; Scala 3 can't, so a value that is `None` only at runtime (`Defs.none`) is an
    * error here. If it can't be told (a class from an earlier incremental run, without `-Yretain-trees`), there's no default.
    */
  def optionDefault(owner: TypeRepr, p: Param, getter: Symbol, call: Term, e: TypeRepr): Expr[Option[Value]] = e.asType match {
    case '[e] =>
      val v = call.asExprOf[Option[e]]
      Expr.summon[ToValue[Option[e]]] match {
        case Some(toV) => '{ val d = $v; if (d.isEmpty) None else Some($toV(d)) }
        case None      =>
          if (defaultIsNone(getter).contains(false)) noToValue(owner, p, getter)
          else '{ None }
      }
  }

  /** Is the default `None` (or `Option.empty`)? `None` = can't tell.
    *
    * The compiler keeps the default getter's body only with `-Yretain-trees` (class members' trees are dropped after typer), so for a class compiled in this run the getter's POSITION decides: it is
    * the point of the default expression, i.e. the start of `None`, the name in `scala.None` / `Option.empty`, and the `(` of a call like `Some(1)`. This is a lexical check of one word at a position
    * the compiler gives, not a re-parse of the parameter (TRIAGE-A): anything but exactly `None`, `scala.None`, `Option.empty` counts as "not None".
    *
    * Known deviations from Scala 2 (which evaluates the default): `= (None)` and `= { None }` count as "not None" (an X-default error, never a dropped `Some`); an unqualified user-defined `None`
    * (`import Shadow.None`) counts as None, so its value is dropped.
    */
  def defaultIsNone(getter: Symbol): Option[Boolean] =
    defaultBody(getter).map(isNoneTree) orElse {
      if (!getter.isDefinedInCurrentRun) None
      else
        getter.pos flatMap { pos =>
          pos.sourceFile.content map { src =>
            // a type application `Option.empty[Int]` has its point at the `[`: the word is the one just before it
            val at     = if (src.startsWith("[", pos.start)) src.lastIndexWhere(!Character.isJavaIdentifierPart(_), pos.start - 1) + 1 else pos.start
            val word   = src.substring(at).takeWhile(Character.isJavaIdentifierPart)
            val before = src.substring(math.max(0, at - 32), at)
            (word == "None" && (!before.endsWith(".") || before.endsWith("scala."))) || (word == "empty" && before.matches("(?s).*\\bOption\\s*\\.\\s*"))
          }
        }
    }

  /** The body of a default getter, if its tree is available (`-Yretain-trees`, or a class local to a block). */
  def defaultBody(getter: Symbol): Option[Term] =
    try {
      getter.tree match {
        case DefDef(_, _, _, Some(rhs)) => Some(rhs)
        case _                          => None
      }
    } catch { case scala.util.control.NonFatal(_) => None }

  /** `None`, `scala.None`, `Option.empty`, `Option.empty[T]` (possibly typed or inlined). */
  def isNoneTree(t: Term): Boolean = t match {
    case Inlined(_, Nil, e) => isNoneTree(e)
    case Typed(e, _)        => isNoneTree(e)
    case Block(Nil, e)      => isNoneTree(e)
    case TypeApply(f, _)    => isNoneTree(f)
    case _                  =>
      t.tpe.widenByName <:< TypeRepr.of[None.type] || {
        val s = t.symbol
        !s.isNoSymbol && s.name == "empty" && s.owner == TypeRepr.of[Option.type].typeSymbol
      }
  }

  /** The companion's `$lessinit$greater$default$N` for `p`. */
  def defaultGetter(owner: TypeRepr, p: Param): Symbol =
    classSymbol(owner).companionModule.methodMember(s"$$lessinit$$greater$$default$$${p.index + 1}").headOption getOrElse {
      report.errorAndAbort(s"no default getter found for ${signature(owner, dealiased = true)}.${p.name}")
    }

  /** `Companion.$lessinit$greater$default$N` (applied to the owner's type arguments if it is generic). */
  def defaultGetterCall(owner: TypeRepr, getter: Symbol): Term = {
    val call = companionRef(owner, classSymbol(owner).companionModule).select(getter)
    if (getter.paramSymss.headOption.exists(_.exists(_.isTypeParam))) call.appliedToTypes(owner.dealias.typeArgs) else call
  }

  /** A reference to `owner`'s companion through `owner`'s prefix: `o.Inner` for `val o: Outer; Json.schema[o.Inner]`, `this.Inner` inside `Outer` (reviewer3 S1: a bare `Ref(companion)` is a malformed
    * tree there). A local class has no prefix. A type projection (`Outer#Inner`) has no path to the companion: the same error as Scala 2 (s2fix2; before, the macro crashed).
    */
  private def companionRef(owner: TypeRepr, companion: Symbol): Term = {
    def prefix(t: TypeRepr): Option[TypeRepr] = t match {
      case AppliedType(tc, _) => prefix(tc)
      case TypeRef(pre, _)    => Some(pre)
      case _                  => None
    }
    prefix(owner.dealias) match {
      case Some(_: NoPrefix) | None     => Ref(companion)
      case Some(pre) if pre.isSingleton => Ref.term(TermRef(pre, companion.name))
      case Some(pre)                    =>
        report.errorAndAbort(
          s"default values of ${owner.dealias.show} are not supported: its companion ${companion.name} is reached through a type projection, not a stable path. " +
            s"Use a path (`val o = new ${pre.typeSymbol.name}; Json.schema[o.${companion.name}]`) or declare it in an object"
        )
    }
  }

  /** Scala 2's X-default error. Like Scala 2, it quotes the default's source when it is known. */
  def noToValue(owner: TypeRepr, p: Param, getter: Symbol): Nothing = {
    val ft     = p.tpe.show(using Printer.TypeReprShortCode) // `Option[Int]`, as Scala 2 prints it
    val owned  = s"${signature(owner, dealiased = true)}(..., ${p.name}: $ft"
    val suffix = defaultBody(getter).map(b => b.pos.sourceCode getOrElse b.show).fold(s"`$owned)`")(c => s"`$owned = $c)`.")
    report.errorAndAbort(
      s"""Can't infer json value for default value of $suffix
         |Please provide `ToValue[$ft]` type class. It can also be automatically derived in case
         |- your json library knows how to convert `$ft` into json and
         |- the bridge between scala-jsonschema and your library is configured. Namely
         | - corresponding library is in classpath (eg. scala-jsonschema-spray-json, scala-jsonschema-play-json, scala-jsonschema-circe-json, etc)
         | - corresponding import is taking place (eg. `import com.github.andyglow.jsonschema.AsSpray._`, `import com.github.andyglow.jsonschema.AsPlay._`, `import com.github.andyglow.jsonschema.AsCirce._` etc)
         |""".stripMargin
    )
  }
}
