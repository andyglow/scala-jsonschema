package com.github.andyglow.jsonschema


private[jsonschema] trait SchemaTypes { this: UContext with UCommons =>
  import c.universe._



  sealed trait SchemaType {
    type Self <: SchemaType

    def tpe: Type

    protected def prefix: Tree

    def extra: SchemaType.Extra
    def withExtra(x: SchemaType.Extra): Self

    final def tree: Tree = {

      val withValidationsApplied = if (extra.validations.nonEmpty) q"$prefix.withValidation(..${extra.validations})" else prefix

      val decorated = (extra.title, extra.description) match {
        case (Some(title), Some(description)) => q"$withValidationsApplied.duplicate(title = Some($title), description = Some($description))"
        case (Some(title), None)              => q"$withValidationsApplied.duplicate(title = Some($title))"
        case (None, Some(description))        => q"$withValidationsApplied.duplicate(description = Some($description))"
        case (None, None)                     => withValidationsApplied
      }

      decorated
    }
  }

  final object SchemaType {
    final case class Extra(
      validations: Seq[Tree] = Seq.empty,
      title: Option[String] = None,
      description: Option[String] = None)

    final case class Bool(extra: Extra = Extra()) extends SchemaType { type Self = Bool; def prefix = q"${N.Schema}.`boolean`()"; val tpe = typeOf[Boolean]; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Integer(extra: Extra = Extra()) extends SchemaType { type Self = Integer; def prefix = q"${N.Schema}.`integer`()"; val tpe = typeOf[Int]; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Number(tpe: Type, extra: Extra = Extra()) extends SchemaType { type Self = Number; def prefix = q"${N.Schema}.`number`[$tpe]()"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Str(tpe: Type, format: Tree, extra: Extra = Extra()) extends SchemaType { type Self = Str; def prefix = q"${N.Schema}.`string`[$tpe]($format)"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Arr(elementTpe: Type, containerTpe: Type, elementSchema: SchemaType, unique: Boolean, extra: Extra = Extra()) extends SchemaType { type Self = Arr; def prefix = q"${N.Schema}.`array`[$elementTpe, $containerTpe](${elementSchema.tree}, $unique)"; val tpe = appliedType(containerTpe, elementTpe); def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Dict(keyTpe: Type, valueTpe: Type, containerTpe: Type, valueSchema: SchemaType, extra: Extra = Extra()) extends SchemaType { type Self = Dict; def prefix = q"${N.Schema}.`dictionary`[$keyTpe, $valueTpe, $containerTpe](${valueSchema.tree})"; val tpe = appliedType(containerTpe, List(keyTpe, valueTpe)); def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Obj(tpe: Type, fields: Seq[Obj.Field], extra: Extra = Extra()) extends SchemaType { type Self = Obj; def prefix = q"${N.Schema}.`object`[$tpe](..${fields map { _.tree }})"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Enum(tpe: Type, values: Seq[Tree], /*duplicates values if created from strings*/names: Option[Seq[String]], extra: Extra = Extra()) extends SchemaType { type Self = Enum; def prefix = q"${N.Schema}.`enum`.of[$tpe](..$values)"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class OneOf(tpe: Type, memberSchema: Seq[SchemaType], extra: Extra = Extra()) extends SchemaType { type Self = OneOf; def prefix = q"${N.Schema}.`oneof`[$tpe](Set(..${memberSchema map { _.tree }}))"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class AllOf(tpe: Type, memberSchema: Seq[SchemaType], extra: Extra = Extra()) extends SchemaType { type Self = AllOf; def prefix = q"${N.Schema}.`allof`[$tpe](Set(..${memberSchema map { _.tree }}))"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Not(tpe: Type, schema: SchemaType, extra: Extra = Extra()) extends SchemaType { type Self = Not; def prefix = q"${N.Schema}.`not`[$tpe](${schema.tree})"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class ValueClass(tpe: Type, innerTpe: Type, schema: SchemaType, extra: Extra = Extra()) extends SchemaType { type Self = ValueClass; def prefix = q"${N.Schema}.`value-class`[$tpe, $innerTpe](${schema.tree})"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Def(tpe: Type, sig: Tree, schema: SchemaType, extra: Extra = Extra()) extends SchemaType { type Self = Def; def prefix = q"${N.Schema}.`def`[$tpe]($sig, ${schema.tree})"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class Ref(tpe: Type, sig: Tree, extra: Extra = Extra()) extends SchemaType { type Self = Ref; def prefix = q"${N.Schema}.`ref`[$tpe]($sig)"; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final case class `-from-tree-`(tpe: Type, prefix: Tree, extra: Extra = Extra()) extends SchemaType { type Self = `-from-tree-`; def withExtra(x: SchemaType.Extra) = copy(extra = x) }
    final object Obj {
      sealed trait Field {
        protected def prefix: Tree
        val description: Option[String]
        def tree: Tree = description.fold(prefix)(d => q"$prefix.withDescription(Some($d))")
        def mapSchema(fn: SchemaType => SchemaType): Field
      }
      final object Field {
        final case class Apply(tpe: Type, name: Tree, schema: SchemaType, required: Option[Tree], default: Option[Tree], description: Option[String]) extends Field {
          def prefix = (required, default) match {
            case (Some(required), Some(default))  => q"${N.Schema}.`object`.Field[$tpe]($name, ${schema.tree}, $required, $default)"
            case (None, Some(default))            => q"${N.Schema}.`object`.Field[$tpe]($name, ${schema.tree}, false, $default)"
            case (Some(required), None)           => q"${N.Schema}.`object`.Field[$tpe]($name, ${schema.tree}, $required)"
            case (None, None)                     => q"${N.Schema}.`object`.Field[$tpe]($name, ${schema.tree})"
          }
          def mapSchema(fn: SchemaType => SchemaType): Field = copy(schema = fn(schema))
        }
        final case class FromJson(tpe: Type, name: Tree, schema: SchemaType, required: Tree, default: Tree, description: Option[String]) extends Field {
          def prefix = q"${N.Schema}.`object`.Field.fromJson[$tpe]($name, ${schema.tree}, $required, $default)"
          def mapSchema(fn: SchemaType => SchemaType): Field = copy(schema = fn(schema))
        }
//        def unapply(x: Tree): Option[Field] = x match {
//          case q"_root_.json.Schema.`object`.Field.apply[$t]($name, $schema)"                         => SchemaType.unapply(schema) map { Field.Apply(t.tpe, name, _, None, None) }
//          case q"_root_.json.Schema.`object`.Field.apply[$t]($name, $schema, $required)"              => SchemaType.unapply(schema) map { Field.Apply(t.tpe, name, _, Some(required), None) }
//          case q"_root_.json.Schema.`object`.Field.apply[$t]($name, $schema, $required, $default)"    => SchemaType.unapply(schema) map { Field.Apply(t.tpe, name, _, Some(required), Some(default)) }
//          case q"_root_.json.Schema.`object`.Field.fromJson[$t]($name, $schema, $required, $default)" => SchemaType.unapply(schema) map { Field.FromJson(t.tpe, name, _, required, default) }
//        }
      }
    }
//    def unapply(x: Tree): Option[SchemaType] = x match {
//      case q"_root_.json.Schema.`boolean`()" => Some(Bool)
//      case q"_root_.json.Schema.`integer`()" => Some(Integer)
//      case q"_root_.json.Schema.`number`[$t]()" => Some(Number(t.tpe))
//      case q"_root_.json.Schema.`string`[$t]($format)" => Some(Str(t.tpe, format))
//      case q"_root_.json.Schema.`set`[$t, $c]($elementSchema)"   => unapply(elementSchema) map { ESet(t.tpe, c.tpe, _) }
//      case q"_root_.json.Schema.`array`[$t, $c]($elementSchema)" => unapply(elementSchema) map { Arr(t.tpe, c.tpe, _) }
//      case q"_root_.json.Schema.`dictionary`[$k, $v, $c]($valueSchema)" => unapply(valueSchema) map { Dict(k.tpe, v.tpe, c.tpe, _) }
//      case q"_root_.json.Schema.`object`[$t](Set(..$fields))" => Obj(t.tpe, fields.map(Obj.Field.unapply(_).get))
//      case q"_root_.json.Schema.`enum`[$t](Set(..$values))"    => Enum(t.tpe, values)
//      case q"_root_.json.Schema.`oneof`[$t](Set(..$schemas))" => OneOf(t.tpe, schemas.map(unapply(_).get))
//      case q"_root_.json.Schema.`allof`[$t](Set(..$schemas))" => AllOf(t.tpe, schemas.map(unapply(_).get))
//      case q"_root_.json.Schema.`not`[$t]($schema)"           => unapply(schema) map { Not(t.tpe, _) }
//      case q"_root_.json.Schema.`ref`[$t]($sig, $schema)"     => unapply(schema) map { Ref(t.tpe, sig, _) }
//      case q"_root_.json.Schema.`value-class`[$o, $i]($schema)" => unapply(schema) map { ValueClass(o.tpe, i.tpe, _) }
//      case q"_root_.json.Schema.`lazy-ref`[$t]($sig)"           => LazyRef(t.tpe, sig)
//    }
  }

  def transformSchema(in: SchemaType)(pf: PartialFunction[SchemaType, SchemaType]): SchemaType = {
    import SchemaType._

    val out = in match {
        case s: Arr        => s.copy(elementSchema = transformSchema(s.elementSchema)(pf))
        case s: Dict       => s.copy(valueSchema = transformSchema(s.valueSchema)(pf))
        case s: OneOf      => s.copy(memberSchema = s.memberSchema map { transformSchema(_)(pf) })
        case s: AllOf      => s.copy(memberSchema = s.memberSchema map { transformSchema(_)(pf) })
        case s: Not        => s.copy(schema = transformSchema(s.schema)(pf))
        case s: Def        => s.copy(schema = transformSchema(s.schema)(pf))
        case s: ValueClass => s.copy(schema = transformSchema(s.schema)(pf))
        case s: Obj        => s.copy(fields = s.fields map { f =>
          f.mapSchema(transformSchema(_)(pf))
        })
        case _              => in
    }

    if (pf.isDefinedAt(out)) pf(out) else out
  }
}
