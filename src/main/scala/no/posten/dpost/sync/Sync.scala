package no.posten.dpost.sync

import java.security.MessageDigest
import java.security.DigestInputStream
import java.io.FileInputStream
import scalaz._
import Scalaz._
import java.io.InputStream
import java.io.ByteArrayInputStream
import org.apache.commons.codec.binary.Hex
import org.apache.commons.io.FileUtils
import java.io.File
import scala.io.Source
import API._

object Sync {

  val listFiles: String => List[File] = { folderName =>
    new java.io.File(folderName).listFiles.toList.filter(_.isFile)
  }

  val createFolders: File => List[String] => Unit = { folder =>
    val files = folder.listFiles

    folderNames => folderNames.foreach { folderName =>
      if (!files.exists(_.getName == folderName)) new File(folder, folderName).mkdir
    }
  }

  import net.liftweb.json._
  import net.liftweb.json.Serialization.{ read, write }

  case class SyncItem(id: String, filename: String, lastUpdated: Long)
  object SyncItem {
    def apply(doc: Document): SyncItem = SyncItem(doc.id, doc.filename, System.currentTimeMillis)
  }

  def writeSyncFile(syncFile: File, content: List[SyncItem]) = {
    val formats = Serialization.formats(NoTypeHints)
    val json = write(content)(formats)
    FileUtils.writeStringToFile(syncFile, json)
  }

  def readSyncFile(syncFile: File) = {
    val ser = Source.fromFile(syncFile, "utf-8").mkString
    implicit val formats = Serialization.formats(NoTypeHints)
    read[List[SyncItem]](ser)
  }

  def findItemsNotIn(ours: List[SyncItem], other: List[SyncItem]) = {
    ours.filterNot(item => other.exists(_.id == item.id))
  }

  def delete(folder: File, syncData: List[SyncItem]): List[SyncItem] = {
    syncData.filter { item =>
      println("Deleting " + item)
      val file = new File(folder, item.filename)
      if (file.exists) file.delete() else true
    }
  }

  def download(folder: File, syncData: List[SyncItem]): List[SyncItem] = {
    syncData.filter { item =>
      println("Downloading " + item)
      val file = new File(folder, item.filename)
      if (!file.exists) {
        API.download(Link(item.id), file).isSuccess
      } else {
        true
      }
    }
  }

  object SyncFolder {
    final val INBOX = "INBOX"
    final val ARCHIVE = "ARCHIVE"
    final val folderNames = List(INBOX, ARCHIVE)
  }
}

case class SHA1(bytes: Array[Byte]) {
  def hex = org.apache.commons.codec.binary.Hex.encodeHexString(bytes)
}

object SHA1 {
  private val BufferSize = 8192

  def apply(str: String): Validation[String, SHA1] = apply(new ByteArrayInputStream(str.getBytes))

  def apply(file: File): Validation[String, SHA1] = for {
    stream <- Control.trap(new FileInputStream(file))
    digest <- apply(stream)
  } yield digest

  def apply(stream: InputStream): Validation[String, SHA1] = Control.trapAndFinally {
    val digest = MessageDigest.getInstance("SHA")
    val dis = new DigestInputStream(stream, digest)
    val buffer = new Array[Byte](BufferSize)
    while (dis.read(buffer) >= 0) {}
    dis.close()
    SHA1(digest.digest)
  } { stream.close() }
}

object Control {
  def trap[A](block: => A): Validation[String, A] = {
    trapAndFinally(block)(Unit)
  }

  def trapAndFinally[A](block: => A)(doFinally: => Unit): Validation[String, A] = {
    try {
      Success(block)
    } catch {
      case e => Failure(e.getMessage + "\n" + e.getStackTraceString)
    } finally {
      doFinally
    }
  }
}
