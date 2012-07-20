package no.posten.dpost.sync

import java.io.File
import Sync._
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils
import scala.io.Source
import scalaz._
import Scalaz._
import API._

object SyncApp extends App {
  def getMessages(account: Account, rel: String) = for {
    documentsLink <- getLink(rel, account.links)
    documents <- GET[Documents](documentsLink)
  } yield documents.document

  def downloadDocumentTofolder(document: Document, folder: File) = {
    val link = getLink("get_document_content", document.links)
    val res = link map { link =>
      val file = new File(folder, document.filename)
      file.createNewFile
      API.download(link, file).map(_ => file)
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
  val localSyncData = readSyncFile(syncFile)

  val documentResult = for {
    _ <- authenticate("18118500008", "Qwer1234")
    entryPoint <- GET[EntryPoint](privateEntryLink)
    archive <- getMessages(entryPoint.primaryAccount, "document_archive")
  } yield archive

  val documents = documentResult | List()
  val remoteSyncData = documents.map(doc => SyncItem(doc))
  val deletedRemote = findItemsNotIn(localSyncData, remoteSyncData)
  val deleted = delete(syncFolder, deletedRemote)
  val localSyncDataAfterDelete = localSyncData -- deleted
  val newRemote = findItemsNotIn(remoteSyncData, localSyncDataAfterDelete)
  val downloaded = Sync.download(syncFolder, newRemote)

  writeSyncFile(syncFile, localSyncDataAfterDelete ++ downloaded)

  //  {
  //    val downloadResults = archive.filterNot(doc => exists(doc, syncData)).map { doc =>
  //      (doc, downloadDocumentTofolder(doc, syncFolder))
  //    }
  //    val downloaded = downloadResults.filter(_._2.isSuccess).map(tup => (tup._1, tup._2.toOption.get))
  //    val newSyncItems = downloaded.map { tup =>
  //      val (doc, file) = tup
  //      SyncItem(getLink("self", doc.links).toOption.get.uri, file.getName, System.currentTimeMillis)
  //    }
  //    writeSyncFile(syncData ++ newSyncItems, syncFile)
  //  }

}