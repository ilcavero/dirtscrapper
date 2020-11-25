package ilcavero

import java.io.{File, PrintWriter}
import java.nio.file.Files

import org.apache.commons.math3.distribution.LogNormalDistribution
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import upickle.default._

import scala.util.{Failure, Success, Try}

object Application extends App {

  println("Starting the scraper....")

  val clubId: String = if(args.isEmpty) {
    println("please pass the club ID as java arg")
    System.exit(1)
    ""
  } else {
    args(0)
  }

  case class Stage(id: String, name: String)

  case class Event(id: String, challengeId: String, name: String, stages: List[Stage])

  case class Championship(id: String, events: List[Event])

  case class Root(championships: List[Championship])

  implicit val sw: ReadWriter[Stage] = macroRW
  implicit val ew: ReadWriter[Event] = macroRW
  implicit val cw: ReadWriter[Championship] = macroRW
  implicit val rw: ReadWriter[Root] = macroRW


  val headers: List[(String, String)] = Files.readString(new File("postheader.txt").toPath)
    .linesIterator
    .toList
    .tail
    .map { line =>
      val name = line.split(':')(0)
      name -> line.substring(name.size + 2)
    }


  val mainTreeJson = requests.get(s"https://dirtrally2.dirtgame.com/api/Club/$clubId/recentResults",  headers = headers, verifySslCerts = false)

  val root = read[Root](mainTreeJson)

  case class LeaderboardEntries(rank: Int, name: String, isDnfEntry: Boolean, vehicleName: String, stageTime: String, stageDiff: String, totalTime: String, totalDiff: String, wheel: Option[Boolean] = None)

  case class Leaderboard(entries: List[LeaderboardEntries])

  case class LeaderboardHolder(championship: Championship, event: Event, stage: Stage, leaderboard: Leaderboard)


  implicit val rwle: ReadWriter[LeaderboardEntries] = macroRW
  implicit val rwl: ReadWriter[Leaderboard] = macroRW

  val championshipId = if(args.size > 1) Some(args(1)) else None
  val eventId = if(args.size > 2) Some(args(2)) else None

  val allLeadeboards: List[LeaderboardHolder] = for {
    championship <- root.championships if championshipId.forall(_ == championship.id)
    event <- championship.events if eventId.forall(_ == event.id)
    stage <- event.stages
  } yield {
    Thread.sleep(463)

    def postBody(wheel: String): String =
      s"""{"challengeId":"${event.challengeId}","selectedEventId":0,"stageId":"${stage.id}","page":1,"pageSize":100,"orderByTotalTime":true,"platformFilter":"None","playerFilter":"Everyone","filterByAssists":"Unspecified","filterByWheel":"$wheel","nationalityFilter":"None","eventId":"${event.id}"}"""

    def getLeaderboard(wheel: String): Leaderboard = {
      Try(requests.post("https://dirtrally2.dirtgame.com/api/Leaderboard", headers = headers, data = postBody(wheel), verifySslCerts = false)) match {
        case Success(resp) =>
          println(s"${championship.id} ${event.id} ${stage.id} $wheel -> Received a ${resp.statusCode}")
          val leaderboard: Leaderboard = read[Leaderboard](resp.text)
          leaderboard
        case Failure(exception) =>
          println(s"${championship.id} ${event.id} ${stage.id} " + exception.getMessage)
          println(postBody(wheel))
          throw exception
      }
    }
    val wheels = getLeaderboard("ON").entries.map(_.name)

    val leaderboard = getLeaderboard("Unspecified")
    val leaderboardWithWheels =leaderboard.copy(leaderboard.entries.map(e => e.copy(wheel = Some(wheels.contains(e.name)))))
    LeaderboardHolder(championship, event, stage, leaderboardWithWheels)
  }

  //adds isLastStage, stageRank, timesInSecs totalStageDrivers, time percentiles

  val hourPattern = raw"""\+?(\d\d):(\d\d):(\d\d\.\d\d\d)""".r
  val timePattern = raw"""\+?(\d\d):(\d\d\.\d\d\d)""".r

  def toTimeDouble(s: String): Double = s match {
    case hourPattern(hours, minutes, seconds) =>
      hours.toDouble * 3600 + minutes.toDouble * 60 + seconds.toDouble
    case timePattern(minutes, seconds) =>
      minutes.toDouble * 60 + seconds.toDouble
    case "--" =>
      0
  }

  var lastStageMap = Map[String, String]()
  var stageTimesMap = Map[String, List[(String, Double, Double, Boolean)]]()
  for {
    lh <- allLeadeboards
    l <- lh.leaderboard.entries
  } {
    lastStageMap = lastStageMap.updated(lh.event.id, lh.stage.id)
    val stageTimeDouble = toTimeDouble(l.stageTime)
    val totalTimeDouble = toTimeDouble(l.totalTime)
    stageTimesMap = stageTimesMap.updatedWith(lh.event.id + lh.stage.id) {
      case Some(rest) =>
        Some(((l.name, stageTimeDouble, totalTimeDouble, l.isDnfEntry) :: rest).sortBy(_._2))
      case None =>
        Some((l.name, stageTimeDouble, totalTimeDouble, l.isDnfEntry) :: Nil)
    }
  }

  val stageTimesDescriptive: Map[String, (LogNormalDistribution, LogNormalDistribution)] = stageTimesMap.map {
    case (key, timeTriples) =>
      val stageStats = new DescriptiveStatistics()
      val totalStats = new DescriptiveStatistics()
      timeTriples.foreach { triple =>
        if (!triple._4) {
          stageStats.addValue(Math.log(triple._2))
          totalStats.addValue(Math.log(triple._3))
        }
      }
      key -> (new LogNormalDistribution(stageStats.getMean, stageStats.getStandardDeviation) -> new LogNormalDistribution(totalStats.getMean, totalStats.getStandardDeviation))
  }

  val f = new File("leaderboard.csv")
  val file = new PrintWriter(f)

  file.println("championship,event,eventName,stage,stageName,rank,name,isDnfEntry,vehicleName,stageTime,stageDiff,totalTime,totalDiff,isLastStage,stageRank,stagePercentile,totalPercentile,wheel")

  for {
    lh <- allLeadeboards
    l <- lh.leaderboard.entries
  } {
    val isLastStage: Boolean = lastStageMap(lh.event.id) == lh.stage.id
    val stageRank: Int = stageTimesMap(lh.event.id + lh.stage.id).indexWhere(_._1 == l.name)
    val (stagePercentile: Double, totalPercentile: Double) = if (!l.isDnfEntry) {
      100 * (1 - stageTimesDescriptive(lh.event.id + lh.stage.id)._1.probability(0, toTimeDouble(l.stageTime))) ->
        100 * (1 - stageTimesDescriptive(lh.event.id + lh.stage.id)._2.probability(0, toTimeDouble(l.totalTime)))
    } else {
      (0d, 0d)
    }
    file.println(List(lh.championship.id, lh.event.id, lh.event.name, lh.stage.id, lh.stage.name, l.rank, l.name, l.isDnfEntry, l.vehicleName,
      toTimeDouble(l.stageTime).formatted("%.3f"), toTimeDouble(l.stageDiff).formatted("%.3f"), toTimeDouble(l.totalTime).formatted("%.3f"), toTimeDouble(l.totalDiff).formatted("%.3f"),
      isLastStage, stageRank, stagePercentile.formatted("%.2f"), totalPercentile.formatted("%.2f"), l.wheel.get.toString).mkString(","))
  }

  file.close()
}

