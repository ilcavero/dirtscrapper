package ilcavero

import upickle.default._

import java.io.{File, PrintWriter}
import java.nio.file.Files

object ClubScrapper extends App {

  val headers: List[(String, String)] = Files.readString(new File("postheader.txt").toPath)
    .linesIterator
    .toList
    .tail
    .map { line =>
      val name = line.split(':')(0)
      name -> line.substring(name.size + 2)
    }

  private def request(page:Int) = requests.post(s"https://dirtrally2.dirtgame.com/api/Club/Search",
    headers = headers, verifySslCerts = false,
    data = s"""{"searchTerm":"","pageNumber":$page,"pageSize":99}""")

  case class Club(id: String, name: String, memberCount: Int, clubAccessType: String)
  case class ResponsePayload(clubs: List[Club])

  implicit val cw: ReadWriter[Club] = macroRW
  implicit val rw: ReadWriter[ResponsePayload] = macroRW

  val f = new File("clubdata.csv")
  val file = new PrintWriter(f)
  file.println("id,name,membercount,accesstype")
  for(i <- 1 to 400) {
    Thread.sleep(100)
    println(s"Sending $i")
    val response = request(i)
    val ResponsePayload(clubs) = read[ResponsePayload](response)
    for(club <- clubs) {
      file.println(List(club.id, club.name, club.memberCount, club.clubAccessType).mkString(","))
    }
    file.flush()
  }
  file.close()
}
