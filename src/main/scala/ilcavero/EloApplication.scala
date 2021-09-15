package ilcavero

import java.io.{File, PrintWriter}
import requests.Session
import upickle.default._

import java.nio.file.{Files, StandardCopyOption}
import scala.util.{Failure, Success, Try}

object EloApplication extends App {

  println("Starting the scraper....")

  val clubId: String = if (args.isEmpty) {
    println("please pass the club ID as java arg")
    System.exit(1)
    ""
  } else {
    args(0)
  }

  val credentialsFile = new File(System.getProperty("ilcavero.credentials", "credentials.txt"))
  val List(email, password) = Files.readString(credentialsFile.toPath).linesIterator.toList
  println(s"using ${credentialsFile.toPath} to login")
  val (s, headers) = Login.login(email, password)
  val eventResults = Scrapper.scrap(clubId, s, headers)
  eventResults.foreach {
    case LeaderboardHolder(championship, event, stage, entries) =>
      val currentElos = EloDb.load(event.id)
      val updatedElos = entries.map { entry =>
        currentElos.get(entry.name) match {
          case Some(currentElo) =>
            val defeatedEntry = entries.filter(_.rank < entry.rank)
            val defeatedByEntry = entries.filter(_.rank > entry.rank)
            val matchUpCount = defeatedEntry.size + defeatedByEntry.size

            val lost = defeatedEntry.foldLeft(0d) {
              case (acum, beater) =>
                val delta = currentElos.get(beater.name) match {
                  case Some(beaterElo) => EloCalculator.delta(currentElo, beaterElo, EloCalculator.Lost)
                  case None => 0
                }
                acum + (delta / matchUpCount)
            }
            val won = defeatedByEntry.foldLeft(0d) {
              case (acum, beater) =>
                val delta = currentElos.get(beater.name) match {
                  case Some(beaterElo) => EloCalculator.delta(currentElo, beaterElo, EloCalculator.Won)
                  case None => 0
                }
                acum + (delta / matchUpCount)
            }
            Some(EloEntry(entry.name, lost + won, event.id))
          case None =>
            println(s"No initial elo found for ${entry.name}")
            None
        }
      }.collect {
        case Some(value) => value
      }
      EloDb.save(event.id, updatedElos)
  }

}

object EloCalculator {
  type Result = Int
  val Won: Result = 1
  val Lost: Result = 0

  val K = 50

  def R(elo: Double): Double = Math.pow(10d, elo / 400)

  def delta(myElo: Double, opponentElo: Double, result: Result): Double = {
    val R1 = R(myElo)
    val R2 = R(opponentElo)
    val E = R1 / (R1 + R2)
    K * (result - E)
  }

}

case class EloEntry(name: String, change: Double, event: String)

object EloDb {
  def load(currentEvent: String): Map[String, Double] = {
    val eloFile = new File(System.getProperty("ilcavero.elo", "elo.csv"))
    var currentElos = Map.empty[String, Double]
    for {
      line <- Files.readString(eloFile.toPath).linesIterator
    } {
      val Array(name, change, event) = line.split(',')
      if (event != currentEvent) {
        currentElos.get(name) match {
          case Some(value) =>
            currentElos = currentElos.updated(name, value + change.toDouble)
          case None =>
            currentElos = currentElos.updated(name, change.toDouble)
        }
      }
    }
    currentElos
  }

  def save(currentEvent: String, entries: Iterable[EloEntry]) = {
    val eloFile = new File(System.getProperty("ilcavero.elo", "elo.csv"))
    val eloTmpFile = new File(System.getProperty("ilcavero.elotmp", "elotmp.csv"))
    val pf = new PrintWriter(eloTmpFile)
    for {
      line <- Files.readString(eloFile.toPath).linesIterator
    } {
      val Array(name, change, event) = line.split(',')
      if (event != currentEvent) {
        pf.println(line)
      }
    }
    for {
      entry <- entries
    } {
      pf.println(f"${entry.name},${entry.change}%.3f,${entry.event}")
    }
    pf.close()
    Files.move(eloTmpFile.toPath, eloFile.toPath, StandardCopyOption.REPLACE_EXISTING)
  }

}

case class CEvent(id: String, eventStatus: String)

case class CChampionship(id: String, events: List[CEvent])

case class Stage(id: String, name: String)

case class Event(id: String, challengeId: String, name: String, stages: List[Stage])

case class Championship(id: String, events: List[Event])

case class Root(championships: List[Championship])

case class LeaderboardEntries(rank: Int, name: String, isDnfEntry: Boolean, vehicleName: String, stageTime: String, stageDiff: String, totalTime: String, totalDiff: String)

case class Leaderboard(entries: List[LeaderboardEntries])

case class LeaderboardHolder(championship: Championship, event: Event, stage: Stage, leaderboard: List[LeaderboardEntries])

object Scrapper {

  implicit val sw: ReadWriter[Stage] = macroRW
  implicit val ew: ReadWriter[Event] = macroRW
  implicit val cw: ReadWriter[Championship] = macroRW
  implicit val rw: ReadWriter[Root] = macroRW
  implicit val cew: ReadWriter[CEvent] = macroRW
  implicit val ccw: ReadWriter[CChampionship] = macroRW

  implicit val rwle: ReadWriter[LeaderboardEntries] = macroRW
  implicit val rwl: ReadWriter[Leaderboard] = macroRW

  def scrap(clubId: String, s: Session, headers: List[(String, String)]): Option[LeaderboardHolder] = {

    val mainTreeJson = s.get(s"https://dirtrally2.dirtgame.com/api/Club/$clubId/recentResults", headers = headers)

    val root = read[Root](mainTreeJson)

    val championshipsJson = s.get(s"https://dirtrally2.dirtgame.com/api/Club/$clubId/championships", headers = headers)
    val championships = read[List[CChampionship]](championshipsJson)

    val championshipAndActiveEvent: Seq[(CChampionship, Option[CEvent])] = championships.map(c => (c, c.events.find(e => e.eventStatus == "Active")))
    val championshipWithActiveEvent: Option[(CChampionship, Option[CEvent])] = championshipAndActiveEvent.find(_._2.nonEmpty)
    championshipWithActiveEvent match {
      case Some((activeChampionship, Some(activeEvent))) =>
        val allLeadeboards: List[LeaderboardHolder] = for {
          championship <- root.championships if championship.id == activeChampionship.id
          event <- championship.events if event.challengeId == activeEvent.id
          stage <- event.stages
        } yield {
          Thread.sleep(400)

          def postBody(page: Int): String =
            s"""{"challengeId":"${event.challengeId}","selectedEventId":0,"stageId":"${stage.id}","page":$page,"pageSize":100,"orderByTotalTime":true,"platformFilter":"None","playerFilter":"Everyone","filterByAssists":"Unspecified","filterByWheel":"Unspecified","nationalityFilter":"None","eventId":"${event.id}"}"""

          var retries = 10

          def getLeaderboard(page: Int): List[LeaderboardEntries] = {
            Try(s.post("https://dirtrally2.dirtgame.com/api/Leaderboard",
              headers = "Content-Type" -> "application/json;charset=utf-8" :: headers, data = postBody(page))) match {
              case Success(resp) =>
                println(s"${championship.id} ${event.id} ${stage.id} $page -> Received a ${resp.statusCode}")
                val entries: List[LeaderboardEntries] = read[Leaderboard](resp.text).entries
                if (entries.size == 100)
                  entries ::: getLeaderboard(page + 1)
                else
                  entries
              case Failure(exception) if retries > 0 =>
                println(s"${championship.id} ${event.id} ${stage.id} $page " + exception.getMessage)
                Thread.sleep(1000)
                retries -= 1
                getLeaderboard(page)
              case Failure(exception) =>
                println(s"${championship.id} ${event.id} ${stage.id} $page " + exception.getMessage)
                println(postBody(page))
                throw exception
            }
          }

          val leaderboard = getLeaderboard(1)
          LeaderboardHolder(championship, event, stage, leaderboard)
        }
        //adds isLastStage, timesInSecs

        val lastStage: String = allLeadeboards.maxBy(_.stage.id.toInt).stage.id

        val startingDrivers: List[LeaderboardEntries] = allLeadeboards.filter(_.stage.id == "1").flatMap(_.leaderboard)

        val finalLeaderboard = allLeadeboards.collect {
          case LeaderboardHolder(championship, event, stage, entries) if stage.id == lastStage =>
            val finishingDrivers = entries.map(e => e.name).toSet
            val dnfRank = entries.count(!_.isDnfEntry) + 1
            val missingDnfs: List[LeaderboardEntries] = startingDrivers.collect {
              case le: LeaderboardEntries if !finishingDrivers.contains(le.name) =>
                LeaderboardEntries(dnfRank, le.name, isDnfEntry = true, vehicleName = le.vehicleName,
                  stageTime = "30:00.000", stageDiff = "+30:00.000", totalTime = "--", totalDiff = "--")
            }
            val fixedRankEntries = entries.map {
              case le if le.isDnfEntry =>
                le.copy(rank = dnfRank)
              case le =>
                le
            }
            LeaderboardHolder(championship, event, stage, fixedRankEntries ++ missingDnfs)
        }
        finalLeaderboard.headOption
      case None =>
        println("No active events")
        None
    }
  }
}