package ilcavero

import java.io.{File, PrintWriter}
import requests.Session
import upickle.default._

import java.nio.file.{Files, StandardCopyOption}
import scala.util.{Failure, Success, Try}

object EloScrapper {

  def processEventsResults(lhs: List[LeaderboardHolder]): Unit = {
    val lastStages: List[LeaderboardHolder] = lhs.groupBy(_.event).map(e => e._1 -> e._2.maxBy(_.stage.id.toInt)).values.toList.sortBy(_.event.id.toInt)
    for {
      lh <- lastStages
    } {
      EloScrapper.processEventResults(lh)
    }
  }

  def processEventResults(eventResults: LeaderboardHolder): Unit = eventResults match {
    case LeaderboardHolder(championship, event, stage, entries, _) =>
      println(s"Calculating elo for ${championship.id} ${event.id} ${stage.id} with ${entries.size} entries")
      val currentElos = EloDb.load(event.id)
      val updatedElos = entries.map { entry =>
        currentElos.get(entry.name) match {
          case Some(currentElo) =>
            val defeatedEntry = entries.filter(e => e.rank < entry.rank && currentElos.contains(e.name) && !e.isDnfEntry)
            val defeatedByEntry = entries.filter(e => e.rank > entry.rank && currentElos.contains(e.name) && !entry.isDnfEntry)
            val matchUpCount = defeatedEntry.size + defeatedByEntry.size

            val lost = defeatedEntry.foldLeft(0d) {
              case (acum, beater) =>
                val delta = currentElos.get(beater.name) match {
                  case Some(beaterElo) => EloCalculator.delta(currentElo, beaterElo, EloCalculator.Lost)
                  case None => 0
                }
                println(f"${entry.name} ($currentElo) lost ${beater.name} (${currentElos.get(beater.name)}) = $delta%.2f / $matchUpCount")
                acum + (delta / matchUpCount)
            }
            val won = defeatedByEntry.foldLeft(0d) {
              case (acum, beaten) =>
                val delta = currentElos.get(beaten.name) match {
                  case Some(beaterElo) => EloCalculator.delta(currentElo, beaterElo, EloCalculator.Won)
                  case None => 0
                }
                println(f"${entry.name} ($currentElo) won ${beaten.name} (${currentElos.get(beaten.name)}) = $delta%.2f / $matchUpCount")
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

  val K = 100

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
      if (event.toInt < currentEvent.toInt) {
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