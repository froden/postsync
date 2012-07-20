package no.posten.dpost.sync

import java.io.File
import Sync._
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils
import scala.io.Source
import scalaz._
import Scalaz._

object SyncApp extends App with API {
  def getMessages(account: Account, rel: String) = for {
    documentsLink <- getLink(rel, account.links)
    documents <- GET[Documents](documentsLink)
  } yield documents.document

  def downloadDocumentTofolder(document: Document, folder: File) = {
    val link = getLink("get_document_content", document.links)
    val res = link map { link =>
      val file = new File(folder, filename(document))
      file.createNewFile
      download(link, file).map(_ => file)
    }
    res.fold(err => err.fail, identity)
  }

  def exists(doc: Document, syncData: List[SyncItem]): Boolean = {
    val id = getLink("self", doc.links).toOption.get.uri
    syncData.exists(_.id == id)
  }

  val syncFolder = new File("/tmp/sync")
  if (!syncFolder.exists) syncFolder.mkdir()
  val syncFile = new File(syncFolder, ".sync")
  if (!syncFile.exists) syncFile.createNewFile()
  val syncData = readSyncFile(syncFile)

  val account = for {
    _ <- authenticate("18118500008", "Qwer1234")
    entryPoint <- GET[EntryPoint](privateEntryLink)
    archive <- getMessages(entryPoint.primaryAccount, "document_archive")
  } yield {
    val downloadResults = archive.filterNot(doc => exists(doc, syncData)).map { doc =>
      (doc, downloadDocumentTofolder(doc, syncFolder))
    }
    val downloaded = downloadResults.filter(_._2.isSuccess).map(tup => (tup._1, tup._2.toOption.get))
    val newSyncItems = downloaded.map { tup =>
      val (doc, file) = tup
      SyncItem(getLink("self", doc.links).toOption.get.uri, file.getName, System.currentTimeMillis)
    }
    writeSyncFile(syncData ++ newSyncItems, syncFile)
  }

}