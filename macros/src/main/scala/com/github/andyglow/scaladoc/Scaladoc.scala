package com.github.andyglow.scaladoc

/** So very minimalistic model of scaladoc
  *
  * @param tags
  */
case class Scaladoc(tags: List[Scaladoc.Tag]) {
  import Scaladoc._
  import Tag._

  def withoutDescription: Scaladoc = copy(tags = tags.filterNot(_.isInstanceOf[Description]))
  def withDescription(x: String): Scaladoc = withoutDescription.copy(tags = tags :+ Description(x))
  lazy val description: Option[String] = tags collectFirst { case Description(x) => x }

  def withoutParam(name: String): Scaladoc = copy(tags = tags.filter { case Param(`name`, _) => false; case _ => true })
  def withParam(name: String, value: String): Scaladoc = withoutParam(name).copy(tags = tags :+ Param(name, value))
  def param(name: String): Option[String] = tags collectFirst { case Param(`name`, x) => x }
  def paramMap: Map[String, String] = tags.collect { case Param(name, x) => (name, x) }.toMap

  def withoutTParam(name: String): Scaladoc = copy(tags = tags.filter { case TParam(`name`, _) => false; case _ => true })
  def withTParam(name: String, value: String): Scaladoc = withoutTParam(name).copy(tags = tags :+ TParam(name, value))
  def tparam(name: String): Option[String] = tags collectFirst { case TParam(`name`, x) => x }
}

object Scaladoc {

  sealed trait Tag
  final object Tag {
    final case class Description(value: String) extends Tag
    final case class Param(name: String, value: String) extends Tag
    final case class TParam(name: String, value: String) extends Tag
  }

  def apply(description: String): Scaladoc = Scaladoc(List(Tag.Description(description)))
}