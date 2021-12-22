package com.github.andyglow.shapeless

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


trait Witness extends Serializable {
  type T
  val value: T {}
}

object Witness extends Dynamic {
  type Aux[T0] = Witness { type T = T0 }

  def mkWitness[A](v: A): Aux[A] = new Witness {
    type T = A
    val value = v
  }

  def selectDynamic(tpeSelector: String): Any = macro SingletonTypeMacros.witnessTypeImpl
}

class SingletonTypeMacros(val c: whitebox.Context) {
  import c.universe._
  import internal._
  import decorators._
  import definitions._

//  def singletonOpsTpe: Type = typeOf[syntax.SingletonOps]

//  def isValueClass(sym: Symbol): Boolean = {
//    val tSym = sym.typeSignature.typeSymbol
//    tSym.isClass && tSym.asClass.isDerivedValueClass
//  }

  private object SingletonType {
    def unapply(value: Tree): Option[Type] = (value, value.tpe) match {
      case (Literal(const), _) => Some(constantType(const))
//      case (_, keyType @ SingleType(_, v)) if !v.isParameter && !isValueClass(v) => Some(keyType)
//      case (q"${sops: Tree}.narrow", _) if sops.tpe <:< singletonOpsTpe =>
//        Some(sops.tpe.member(TypeName("T")).typeSignature)
      case _ => None
    }
  }

  private def parseLiteralType(typeStr: String): Option[Type] = for {
    parsed  <- util.Try(c.parse(typeStr)).toOption
    checked <- Option(c.typecheck(parsed, silent = true)) if checked.nonEmpty
    tpe     <- SingletonType.unapply(checked)
  } yield tpe

  private def fieldTypeCarrier(tpe: Type): Literal =
    mkTypeCarrier(tq"""{
      type T = $tpe
    }""")

  private def mkTypeCarrier(tree: Tree): Literal = {
    val carrier = c.typecheck(tree, mode = c.TYPEmode).tpe

    // We can't yield a useful value here, so return Unit instead which is at least guaranteed
    // to result in a runtime exception if the value is used in term position.
    Literal(Constant(())).setType(carrier)
  }

  def witnessTypeImpl(tpeSelector: Tree): Tree = {
    val q"${tpeString: String}" = (tpeSelector: @unchecked)
    val tpe = parseLiteralType(tpeString)
      .getOrElse(c.abort(c.enclosingPosition, s"Malformed literal $tpeString"))

    fieldTypeCarrier(tpe)
  }
}