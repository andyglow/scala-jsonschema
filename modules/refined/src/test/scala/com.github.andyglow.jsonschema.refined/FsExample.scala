package com.github.andyglow.jsonschema.refined

import com.github.andyglow.json.JsonFormatter
import com.github.andyglow.jsonschema.AsValue

import json.Json
import json.schema._

import com.github.andyglow.jsonschema.RefinedSupport._

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._

object FsExample {
  import FsExampleJsonSchema._

  def main(args: Array[String]): Unit = println {
    fsSchema.stringify(Version.Draft07(id = "http://models.org/example.json"))
  }
}

object FsExampleMsg {

  sealed trait FsType
  object FsType {
    case object ext3  extends FsType
    case object ext4  extends FsType
    case object btrfs extends FsType
  }

  sealed trait DiskType
  object DiskType {
    case object disk  extends DiskType
    case object nfs   extends DiskType
    case object tmpfs extends DiskType
  }

  @discriminator sealed trait Storage
  object Storage {
    // format: off
    @definition("diskDevice") @discriminatorKey("disk")  final case class DiskDevice(device: String Refined MatchesRegex[W.`"^/dev/[^/]+(/[^/]+)*$"`.T]) extends Storage
    @definition("diskUUID")   @discriminatorKey("disk")  final case class DiskUUID(label: java.util.UUID) extends Storage
    @definition("nfs")        @discriminatorKey("nfs")   final case class NFS(remotePath: String Refined MatchesRegex[W.`"^(/[^/]+)+$"`.T], server: String) extends Storage
    @definition("tmpfs")      @discriminatorKey("tmpfs") final case class TmpFS(sizeInMB: Refined[Int, GreaterEqual[W.`16`.T] And LessEqual[W.`512`.T]]) extends Storage
    // format: on
  }

  case class FS(
    storage: Storage,
    fstype: Option[FsType],
    readonly: Option[Boolean],
    options: Option[Set[String] Refined MinSize[W.`1`.T]]
  )

}

object FsExampleJsonSchema {
  import FsExampleMsg._

  val fsSchema: json.Schema[FS] = Json.schema[FS]
}
