package no.posten.dpost.sync

import scala.Array.canBuildFrom

import org.apache.commons.io.FileUtils
import org.junit.rules.TemporaryFolder
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import Sync.SyncFolder
import Sync.createFolders
import Sync.listFiles

@RunWith(classOf[JUnitRunner])
class SyncTest extends FunSpec with ShouldMatchers {
  val tempFolder = new TemporaryFolder
  tempFolder.create()
  val testfilename1 = "testfile1"
  val testfile1 = tempFolder.newFile(testfilename1)
  FileUtils.writeStringToFile(testfile1, "Dette er en test")
  tempFolder.newFile("testfile2")
  tempFolder.newFolder

  describe("tempfolder") {
    it("should have 2 files and a folder") {
      tempFolder.getRoot.listFiles.length should be(3)
    }
  }

  describe("sync") {
    it("should only list files") {
      val files = listFiles(tempFolder.getRoot.getAbsolutePath)
      files.length should be(2)
    }

    it("should create the digipost folders") {
      val folder = tempFolder.getRoot
      folder.listFiles.map(_.getName) should not contain (SyncFolder.INBOX)
      createFolders(folder)(SyncFolder.folderNames)
      folder.listFiles.map(_.getName) should contain(SyncFolder.INBOX)
    }
  }

  describe("SHA1") {
    it("should calculate SHA1 of empty string") {
      SHA1("").fold(error => fail(error), sha => sha.hex should equal("da39a3ee5e6b4b0d3255bfef95601890afd80709"))
    }

    it("should give the sha1 hash of a file") {
      SHA1(testfile1).fold(error => fail(error), sha => sha.hex should equal("5a27363f823ecf20a7722f445a35aa77bd5ea236"))
    }
  }
}
