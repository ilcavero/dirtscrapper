package ilcavero

import java.io.{File, PrintWriter}
import org.apache.commons.math3.distribution.LogNormalDistribution
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import upickle.default._

import java.nio.file.Files
import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Application extends App {

  println("Starting the scraper....")

  val clubId: String = if (args.isEmpty) {
    println("please pass the club ID as java arg")
    System.exit(1)
    ""
  } else {
    args(0)
  }

  private val credentialsFile = new File(System.getProperty("ilcavero.credentials", "credentials.txt"))
  val (s, headers) = if (credentialsFile.exists()) {
    val credentials = Files.readString(credentialsFile.toPath).linesIterator.toList
    println(s"using ${credentialsFile.toPath} to login")
    Login.login(credentials(0), credentials(1))
  } else {
    println("Invalid credentials file, looking for postheader.txt")
    val postHeaderPath = new File("postheader.txt").toPath
    requests.Session() ->
      Files.readString(postHeaderPath)
        .linesIterator
        .toList
        .tail
        .map { line =>
          val name = line.split(':')(0)
          name -> line.substring(name.size + 2)
        }
  }

  case class CEvent(id: String, eventStatus: String)
  case class CChampionship(id: String, events: List[CEvent])

  case class Stage(id: String, name: String)
  case class Event(id: String, challengeId: String, name: String, stages: List[Stage])
  case class Championship(id: String, events: List[Event])
  case class Root(championships: List[Championship])

  implicit val sw: ReadWriter[Stage] = macroRW
  implicit val ew: ReadWriter[Event] = macroRW
  implicit val cw: ReadWriter[Championship] = macroRW
  implicit val rw: ReadWriter[Root] = macroRW
  implicit val cew: ReadWriter[CEvent] = macroRW
  implicit val ccw: ReadWriter[CChampionship] = macroRW



  val mainTreeJson = s.get(s"https://dirtrally2.dirtgame.com/api/Club/$clubId/recentResults", headers = headers)

  val root = read[Root](mainTreeJson)

  case class LeaderboardEntries(rank: Int, name: String, isDnfEntry: Boolean, vehicleName: String, stageTime: String, stageDiff: String, totalTime: String, totalDiff: String, wheel: Option[Boolean] = None)
  case class Leaderboard(entries: List[LeaderboardEntries])
  case class LeaderboardHolder(championship: Championship, event: Event, stage: Stage, leaderboard: List[LeaderboardEntries])

  implicit val rwle: ReadWriter[LeaderboardEntries] = macroRW
  implicit val rwl: ReadWriter[Leaderboard] = macroRW

  val all: String => Boolean = _ => true
  def matchId(id: String): String => Boolean = that => id == that


  val (championshipFilter, eventFilter) = {
    if(args.size > 2) {
      println("downloading specified championship and event IDs")
      matchId(args(1)) -> matchId(args(2))
    } else if(args.size > 1 && args(1).startsWith("auto")) {
      val championshipsJson = s.get(s"https://dirtrally2.dirtgame.com/api/Club/$clubId/championships", headers = headers)
      val championships = read[List[CChampionship]](championshipsJson)
      val (championship, Some(activeEvent)) = championships.map(c => (c, c.events.find(e => e.eventStatus == "Active")))
          .find(_._2.nonEmpty)
          .getOrElse(throw new IllegalStateException("no active events, cannot use auto mode"))
      val (cId, eChallengeId) = if(args(1) == "auto") {
        println("downloading active event")
        (championship.id, activeEvent.id)
      } else {
        println("downloading last active event")
        val previousEvent = championship.events.takeWhile(e => e != activeEvent).lastOption.getOrElse(throw new IllegalStateException("No previous active event"))
        (championship.id, previousEvent.id)
      }
      val eId = root.championships.flatMap(_.events).find(_.challengeId == eChallengeId).map(_.id).get
      matchId(cId) -> matchId(eId)
    } else if(args.size > 1 && args(1) == "all") {
      ((_: String) => true, (_: String) => true)
    } else {
      for {
        championship <- root.championships
      } {
        println("Found championship: " + championship.id)
        championship.events.map(e => s"    ${e.id}:${e.name}:${e.stages.size - 1}").foreach(println)
      }
      @tailrec
      def readNotEmpty(prompt: String): String = {
        val in = StdIn.readLine(prompt)
        if (in.isEmpty) readNotEmpty(prompt) else in
      }
      def parse(x: String): String => Boolean = x match {
        case "-" => all
        case s if s.toIntOption.isDefined => matchId(s)
        case s => throw new IllegalArgumentException(s"$s not a number")
      }
      println("Enter - for no filter")
      val cFilter: String => Boolean = if(args.size > 1) {
        println(s"Matching championship ${args(1)}")
        id => id == args(1)
      } else {
        parse(readNotEmpty(s"Enter championship id: "))
      }
      val eFilter = parse(readNotEmpty(s"Enter event id: "))
      (cFilter, eFilter)
    }
  }

  val allLeadeboards: List[LeaderboardHolder] = for {
    championship <- root.championships if championshipFilter(championship.id)
    event <- championship.events if eventFilter(event.id)
    stage <- event.stages
  } yield {
    Thread.sleep(400)

    def postBody(wheel: String, page: Int): String =
      s"""{"challengeId":"${event.challengeId}","selectedEventId":0,"stageId":"${stage.id}","page":$page,"pageSize":100,"orderByTotalTime":true,"platformFilter":"None","playerFilter":"Everyone","filterByAssists":"Unspecified","filterByWheel":"$wheel","nationalityFilter":"None","eventId":"${event.id}"}"""

    def getLeaderboard(wheel: String, page: Int): List[LeaderboardEntries] = {
      Try(s.post("https://dirtrally2.dirtgame.com/api/Leaderboard",
        headers = "Content-Type" -> "application/json;charset=utf-8" :: headers, data = postBody(wheel, page))) match {
        case Success(resp) =>
          println(s"${championship.id} ${event.id} ${stage.id} $page $wheel -> Received a ${resp.statusCode}")
          val entries: List[LeaderboardEntries] = read[Leaderboard](resp.text).entries
          if (entries.size == 100)
            entries ::: getLeaderboard(wheel, page + 1)
          else
            entries
        case Failure(exception) =>
          println(s"${championship.id} ${event.id} ${stage.id} $page " + exception.getMessage)
          println(postBody(wheel, page))
          throw exception
      }
    }

    val wheelsOnly: List[String] = getLeaderboard("ON", 1).map(_.name)

    val leaderboard = getLeaderboard("Unspecified", 1)
    val leaderboardWithWheels = leaderboard.map(e => e.copy(wheel = Some(wheelsOnly.contains(e.name))))
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

  case class StageResult(driverName: String, stageTime: Double, totalTime: Double, isDnf: Boolean)

  var stageTimesMap = Map[(String, String), List[StageResult]]()
  for {
    lh <- allLeadeboards
    l <- lh.leaderboard
  } {
    lastStageMap = lastStageMap.updated(lh.event.id, lh.stage.id)
    val stageTimeDouble = toTimeDouble(l.stageTime)
    val totalTimeDouble = toTimeDouble(l.totalTime)
    stageTimesMap = stageTimesMap.updatedWith(lh.event.id -> lh.stage.id) {
      case Some(rest) =>
        Some(StageResult(l.name, stageTimeDouble, totalTimeDouble, l.isDnfEntry) :: rest)
      case None =>
        Some(StageResult(l.name, stageTimeDouble, totalTimeDouble, l.isDnfEntry) :: Nil)
    }
  }

  val startingDrivers: Map[(Championship, Event), Set[(String, Boolean, String)]] = {
    var result: Map[(Championship, Event), Set[(String, Boolean, String)]] = Map.empty.withDefaultValue(Set.empty)
    for {
      LeaderboardHolder(championship, event, stage, entries) <- allLeadeboards if entries.nonEmpty && stage.id != lastStageMap(event.id)
    } {
      val startingDriverInfo: Set[(String, Boolean, String)] = entries.map {
        entry => (entry.name, entry.wheel.get, entry.vehicleName)
      }.toSet
      result = result.updated(championship -> event, result(championship -> event) ++ startingDriverInfo)
    }
    result
  }

  val allLeaderboardsPlusDnf = allLeadeboards.map {
    case LeaderboardHolder(championship, event, stage, entries) if entries.nonEmpty && stage.id == lastStageMap(event.id) =>
      val stageStartingDrivers: Set[(String, Boolean, String)] = startingDrivers(championship -> event)
      val runningDrivers = entries.map(e => e.name).toSet
      var dnfRank = runningDrivers.size
      val missingDnfs: Set[LeaderboardEntries] = stageStartingDrivers.collect {
        case (name, wheel, vehicle) if !runningDrivers.contains(name) =>
          dnfRank += 1
          val totaltimeMin = 30 * (stage.id.toInt + 1)
          val hourTotalTime = totaltimeMin / 60
          val minTotalTime = totaltimeMin % 60
          val totalTime = hourTotalTime.formatted("%02d") + ":" + minTotalTime.formatted("%02d") + ":00.000"
          LeaderboardEntries(dnfRank, name, isDnfEntry = true, vehicleName = vehicle,
            stageTime = "30:00.000", stageDiff = "+30:00.000", totalTime = totalTime, totalDiff = "+" + totalTime, wheel = Some(wheel))
      }
      LeaderboardHolder(championship, event, stage, entries ++ missingDnfs)
    case lh => lh
  }

  val stageTimesDescriptive: Map[(String, String), (LogNormalDistribution, LogNormalDistribution)] = stageTimesMap.map {
    case (key, stageResults) if stageResults.count(!_.isDnf) > 1 =>
      def createDistribution(getTime: StageResult => Double): LogNormalDistribution = {
        val stats = new DescriptiveStatistics()
        val reasonableMaxMultiplier = 1.22 // Times beyond this will not be part of the model and will get 0.0
        val reasonableMax = stageResults.filter(!_.isDnf).map(getTime).min * reasonableMaxMultiplier
        stageResults.foreach { result =>
          if (!result.isDnf && getTime(result) < reasonableMax) {
            stats.addValue(Math.log(getTime(result)))
          }
        }
        new LogNormalDistribution(stats.getMean, stats.getStandardDeviation)
      }
      val stageDistribution = createDistribution(_.stageTime)
      val totalDistribution = createDistribution(_.totalTime)
      key -> (stageDistribution -> totalDistribution)
    case (key, stageResults) =>
      (key, (new LogNormalDistribution(stageResults.minBy(_.stageTime).stageTime, 1), new LogNormalDistribution(stageResults.minBy(_.totalTime).totalTime, 1)))
  }

  val f = new File(System.getProperty("ilcavero.output", "leaderboarddata.csv"))
  val file = new PrintWriter(f)

  file.println("championship,event,eventName,stage,stageName,rank,name,isDnfEntry,vehicleName,stageTime,stageDiff,totalTime,totalDiff,isLastStage,stageRank,stagePercentile,totalPercentile,wheel,group,drive")

  allLeaderboardsPlusDnf.foreach { lh =>
    var dnfIndex = 0

    for {
      l <- lh.leaderboard
    } {
      val isLastStage: Boolean = lastStageMap(lh.event.id) == lh.stage.id
      val stageTimes = stageTimesMap(lh.event.id -> lh.stage.id).sortBy(_.stageTime)
      val stageRank: Int = {
        val index = stageTimes.indexWhere(_.driverName == l.name)
        if (index == -1) {
          dnfIndex += 1
          stageTimes.size + dnfIndex
        } else {
          index + 1
        }
      }
      val (stagePercentile: Double, totalPercentile: Double) = if (!l.isDnfEntry) {
        100 * (1 - stageTimesDescriptive(lh.event.id -> lh.stage.id)._1.probability(0, toTimeDouble(l.stageTime))) ->
          100 * (1 - stageTimesDescriptive(lh.event.id -> lh.stage.id)._2.probability(0, toTimeDouble(l.totalTime)))
      } else {
        (0d, 0d)
      }
      val group = cars(l.vehicleName)
      val drive = groups(group)
      file.println(List(lh.championship.id, lh.event.id, lh.event.name, lh.stage.id.toInt + 1, lh.stage.name, l.rank, l.name, l.isDnfEntry, l.vehicleName,
        toTimeDouble(l.stageTime).formatted("%.3f"), toTimeDouble(l.stageDiff).formatted("%.3f"), toTimeDouble(l.totalTime).formatted("%.3f"), toTimeDouble(l.totalDiff).formatted("%.3f"),
        isLastStage, stageRank, stagePercentile.formatted("%.2f"), totalPercentile.formatted("%.2f"), l.wheel.get.toString, group, drive).mkString(","))
    }
  }

  file.close()

  lazy val cars = Map(
    "MINI Cooper S" -> "H1 (FWD)",
    "Lancia Fulvia HF" -> "H1 (FWD)",
    "DS 21" -> "H1 (FWD)",
    "Volkswagen Golf GTI 16V" -> "H2 (FWD)",
    "Peugeot 205 GTI" -> "H2 (FWD)",
    "Ford Escort Mk II" -> "H2 (RWD)",
    "Alpine Renault A110 1600 S" -> "H2 (RWD)",
    "Fiat 131 Abarth Rally" -> "H2 (RWD)",
    "Opel Kadett C GT/E" -> "H2 (RWD)",
    "BMW E30 M3 Evo Rally" -> "H3 (RWD)",
    "Opel Ascona 400" -> "H3 (RWD)",
    "Lancia Stratos" -> "H3 (RWD)",
    "Renault 5 Turbo" -> "H3 (RWD)",
    "Datsun 240Z" -> "H3 (RWD)",
    "Ford Sierra Cosworth RS500" -> "H3 (RWD)",
    "Lancia 037 Evo 2" -> "Group B (RWD)",
    "Opel Manta 400" -> "Group B (RWD)",
    "BMW M1 Procar Rally" -> "Group B (RWD)",
    "Porsche 911 SC RS" -> "Group B (RWD)",
    "Audi Sport quattro S1 E2" -> "Group B (4WD)",
    "Peugeot 205 T16 Evo 2" -> "Group B (4WD)",
    "Lancia Delta S4" -> "Group B (4WD)",
    "Ford RS200" -> "Group B (4WD)",
    "MG Metro 6R4" -> "Group B (4WD)",
    "Ford Fiesta R2" -> "R2",
    "Opel Adam R2" -> "R2",
    "Peugeot 208 R2" -> "R2",
    "Peugeot 306 Maxi" -> "F2 Kit Car",
    "Seat Ibiza Kit Car" -> "F2 Kit Car",
    "Volkswagen Golf Kitcar" -> "F2 Kit Car",
    "Mitsubishi Lancer Evolution VI" -> "Group A",
    "SUBARU Impreza 1995" -> "Group A",
    "Lancia Delta HF Integrale" -> "Group A",
    "Ford Escort RS Cosworth" -> "Group A",
    "SUBARU Legacy RS" -> "Group A",
    "SUBARU WRX STI NR4" -> "NR4/R4",
    "Mitsubishi Lancer Evolution X" -> "NR4/R4",
    "Ford Focus RS Rally 2001" -> "2000cc",
    "SUBARU Impreza (2001)" -> "2000cc",
    "Citroën C4 Rally" -> "2000cc",
    "ŠKODA Fabia Rally" -> "2000cc",
    "Ford Focus RS Rally 2007" -> "2000cc",
    "SUBARU Impreza" -> "2000cc",
    "Peugeot 206 Rally" -> "2000cc",
    "SUBARU Impreza S4 Rally" -> "2000cc",
    "Ford Fiesta R5" -> "R5",
    "Ford Fiesta R5 MKII" -> "R5",
    "Peugeot 208 T16 R5" -> "R5",
    "Mitsubishi Space Star R5" -> "R5",
    "ŠKODA Fabia R5" -> "R5",
    "Citroën C3 R5" -> "R5",
    "Volkswagen Polo GTI R5" -> "R5",
    "Chevrolet Camaro GT4.R" -> "Rally GT",
    "Porsche 911 RGT Rally Spec" -> "Rally GT",
    "Aston Martin V8 Vantage GT4" -> "Rally GT",
    "Ford Mustang GT4" -> "Rally GT",
    "BMW M2 Competition" -> "Rally GT"
  )

  lazy val groups = Map(
    "H1 (FWD)" -> "FWD",
    "H2 (FWD)" -> "FWD",
    "H2 (RWD)" -> "RWD",
    "H3 (RWD)" -> "RWD",
    "Group B (RWD)" -> "RWD",
    "Group B (4WD)" -> "4WD",
    "R2" -> "FWD",
    "F2 Kit Car" -> "FWD",
    "Group A" -> "4WD",
    "NR4/R4" -> "4WD",
    "2000cc" -> "4WD",
    "R5" -> "4WD",
    "Rally GT" -> "RWD",
  )
}