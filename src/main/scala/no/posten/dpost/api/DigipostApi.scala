package no.posten.dpost.api

import dispatch._
import net.liftweb.json._
import net.liftweb.json.Serialization.write
import java.util
import com.ning.http.client
import client.{RequestBuilder, Cookie}
import scala.collection.JavaConverters._

sealed trait Resource {
  val link: List[Link]

  def links: List[Link] = link
  def link(relation: String) = links.find(_.rel.endsWith(relation))
}

case class Link(val rel: String, val uri: String, val `media-type`: String)
object Link {
  def apply(uri: String) = new Link(null, uri, null)
}

case class EntryPoint(val csrfToken: String, val link: List[Link], val primaryAccount: Account) extends Resource

case class Accounts(account: List[Account])
case class Account(fullName: String, email: List[String], link: List[Link]) extends Resource

case class Documents(document: List[Document])
case class Document(subject: String, creatorName: String, link: List[Link]) extends Resource


case class Authentication(username: String, password: String)

trait DigipostApiClient {

  final val DigipostJsonV2 = "application/vnd.digipost-v2+json"

  val digipostUri = "https://www.digipost.no/post/api"

  def toJson(obj: AnyRef): String = write(obj)(Serialization.formats(NoTypeHints))
  def fromJson[T: Manifest](json: String): T = JsonParser.parse(json).extract[T](DefaultFormats, manifest[T])

  var cookies: List[Cookie] = Nil

  def withCookies(req: RequestBuilder) = {
    cookies.foreach(cookie => req.addOrReplaceCookie(cookie))
    req
  }

  def authenticate(authentication: Authentication): Promise[Either[String, Unit]] = {
    val loginUrl = url(digipostUri) / "private/passwordauth"
    val json = toJson(authentication)
    val res = Http(loginUrl.POST.addHeader("Content-Type", DigipostJsonV2).setBody(json).OK(res => cookies = res.getCookies.asScala.toList)).either
    for (error <- res.left) yield "Login failed: " + error.getMessage
  }

  def GET[T: Manifest](link: Link): Promise[Either[String, T]] = {
    val headers = Map("Accept" -> DigipostJsonV2)
    val req = withCookies(url(link.uri))
    val res = Http(req <:< headers OK(res => fromJson[T](res.getResponseBody))).either
    for (error <- res.left) yield "GET request for: %s failed: %s".format(link.uri, error.getMessage)
  }

}

object Test extends App with DigipostApiClient {
  val res = for {
    _ <- authenticate(Authentication("fnr", "pass"))
    entryPointRes <- GET[EntryPoint](Link(digipostUri + "/private"))
  } yield for {
      entryPoint <- entryPointRes.right
  } yield {
    println(entryPoint)
    val primaryAccount = entryPoint.primaryAccount
    println(primaryAccount)
    val areas = List("document_inbox", "document_workarea", "document_archive")
    val docs = areas.map { rel =>
      GET[Documents](primaryAccount.link(rel).get)
    }
    for (d <- Promise.all(docs)) yield for {
      docEither <- d
    } yield for (docs <- docEither.right) yield for (doc <- docs.document) println(doc)
  }
}
