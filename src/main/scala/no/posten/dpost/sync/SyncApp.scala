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
  val localSyncDataAfterDownload = localSyncDataAfterDelete ++ downloaded
  val newFiles = findFilesNotIn(syncFolder, localSyncDataAfterDownload)
  val uploadedFiles = upload(newFiles)

  writeSyncFile(syncFile, localSyncDataAfterDownload)

}