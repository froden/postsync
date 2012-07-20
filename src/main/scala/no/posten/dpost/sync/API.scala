package no.posten.dpost.sync

import dispatch._
import net.liftweb.json.DefaultFormats
import scalaz._
import Scalaz._
import net.liftweb.json.JsonParser
import java.io.File
import java.io.OutputStream
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.InputStream

case class EntryPoint(val csrfToken: String, val link: List[Link], val primaryAccount: Account) {
  def links = link
}
case class Link(val rel: String, val uri: String, val `media-type`: String)
object Link {
  def apply(uri: String) = new Link(null, uri, null)
}
case class Accounts(val account: List[Account])
case class Account(val fullName: String, val email: String, val link: List[Link]) {
  def links = link
}
case class Documents(val document: List[Document])
case class Document(val subject: String, val creatorName: String, val fileType: String, val link: List[Link]) {
  def links = link
}

trait API {
  implicit val formats = DefaultFormats // Brings in default date formats etc.

  val baseUrl = "http://localhost:8080"
  lazy val authUrl = baseUrl + "/api/private/passwordauth"
  lazy val authLink = Link(authUrl)
  lazy val privateEntryUrl = baseUrl + "/api/private"
  lazy val privateEntryLink = Link(privateEntryUrl)

  val http = new Http with thread.Safety

  final val DigipostJsonV2 = "application/vnd.digipost-v2+json"
  final val UrlEncodedFormData = "application/x-www-form-urlencoded"

  def authenticate(username: String, password: String) = {
    val parameters = Seq("foedselsnummer" -> username, "passord" -> password)
    val headers = Map("Content-Type" -> UrlEncodedFormData)
    Control.trap {
      http(url(baseUrl) / "passordautentisering" << parameters <:< headers >- {
        println(_)
      })
    }
  }

  def GET[T: Manifest](link: Link) = {
    val headers = Map("Accept" -> DigipostJsonV2)
    Control.trap {
      http(url(link.uri) <:< headers >- { json =>
        println(json)
        JsonParser.parse(json).extract[T]
      })
    }
  }

  def download(link: Link, toFile: File) = {
    val writeToStream: OutputStream => InputStream => Unit = out => (in => IOUtils.copy(in, out))
    val headers = Map("Accept" -> DigipostJsonV2)
    for {
      toStream <- Control.trap(FileUtils.openOutputStream(toFile))
      _ <- Control.trapAndFinally(http(url(link.uri).gzip <:< headers >> writeToStream(toStream)))(toStream.close)
    } yield ()
  }

  def getLink(relation: String, links: List[Link]) = links.find(_.rel.endsWith(relation)).toSuccess("No %s link found".format(relation))
}