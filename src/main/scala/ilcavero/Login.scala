package ilcavero

import requests.{RequestBlob, Session}
import upickle.default._

import scala.util.Try

object Login {

  val headers = List(
    "User-Agent" -> "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:87.0) Gecko/20100101 Firefox/87.0",
    "Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Language" -> "en-US,en;q=0.5")

  val acceptJsonHeaders = List(
    "User-Agent" -> "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:87.0) Gecko/20100101 Firefox/87.0",
    "Accept" -> "application/json, text/plain, */*",
    "Accept-Language" -> "en-US,en;q=0.5")

  def login(email: String, password: String): (Session, List[(String, String)]) = {
    val s = requests.Session()
    val response = s.get("https://dirtrally2.dirtgame.com/account/login?returnUri=%2F", keepAlive = true)
    val responseText = response.text()
    val start = responseText.indexOf("""<input name="__RequestVerificationToken" type="hidden" value="""")  + """<input name="__RequestVerificationToken" type="hidden" value="""".length
    val end = responseText.indexOf("\"", start)
    val formToken = responseText.substring(start, end)
    Try(s.post(url = response.url, keepAlive = true,
      headers = ("Origin" -> "https://accounts.codemasters.com") :: ("Referer" -> response.url) :: headers,
      data = RequestBlob.FormEncodedRequestBlob(List("DisableLinks" -> "False",
        "Email" -> email, "Password" -> password, "RememberMe" -> "false", "__RequestVerificationToken" -> formToken))))
    val initialState = s.get(url = "https://dirtrally2.dirtgame.com/api/ClientStore/GetInitialState", headers = acceptJsonHeaders, keepAlive = true)
    implicit val cw: ReadWriter[InitialStateIdentity] = macroRW
    implicit val rw: ReadWriter[InitialStateRoot] = macroRW
    val xsrfh = read[InitialStateRoot](initialState).identity.token
    s -> ("RaceNet.XSRFH" -> xsrfh :: acceptJsonHeaders)
  }
}
case class InitialStateIdentity(token: String)
case class InitialStateRoot(identity: InitialStateIdentity)
