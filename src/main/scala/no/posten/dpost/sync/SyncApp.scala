package no.posten.dpost.sync

import java.io.File

object SyncApp extends App with API {
  def getMessages(account: Account, rel: String) = for {
    documentsLink <- getLink(rel, account.links)
    documents <- GET[Documents](documentsLink)
  } yield documents.document

  def downloadDocumentTofolder(document: Document, folder: File) = {
    val link = getLink("get_document_content", document.links)
    link map { link =>
      val file = new File(folder, document.subject)
      file.createNewFile
      download(link, file)
    }
  }

  //////

  //Sync.createFolders(new File("/tmp"))(Sync.SyncFolder.folderNames)

  val account = for {
    _ <- authenticate("18118500008", "Qwer1234")
    entryPoint <- GET[EntryPoint](privateEntryLink)
  } yield entryPoint.primaryAccount

  account fold (println, println)

  //val inbox = account map (a => getMessages(a, "inbox"))
  //val workarea = account map (a => getMessages(a, "document_workarea"))
  for {
    account <- account
    archive <- getMessages(account, "document_archive")
  } yield {
    println(archive)
    archive.foreach(doc => downloadDocumentTofolder(doc, new File("/tmp")))
  }

}