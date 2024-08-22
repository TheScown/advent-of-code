package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec

case class Day4(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val records = parse().sorted

    val minuteMap = buildMinuteMap(records, Map())
    val worstGuard = minuteMap.maxBy(p => p._2.values.sum)._1
    val result = minuteMap(worstGuard).maxBy(p => p._2)._1 * worstGuard

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val records = parse().sorted

    val minuteMap = buildMinuteMap(records, Map())
    val worstMinute = minuteMap.filter(_._2.nonEmpty).map { case (id, map) =>
      (id, map.maxBy(_._2))
    }.maxBy(_._2._2)
    val result = worstMinute._1 * worstMinute._2._1

    println(s"Result 2: $result")
  }

  @tailrec
  private def buildMinuteMap(remainingRecords: Vector[Record], acc: Map[Int, Map[Int, Int]]): Map[Int, Map[Int, Int]] = {
    if (remainingRecords.isEmpty) acc
    else {
      val currentGuard = remainingRecords.head.entry match {
        case BeginShift(id) => id
        case _ => throw new IllegalStateException(s"Invalid split: $remainingRecords")
      }

      val (currentGuardEntries, rest) = remainingRecords.tail.span(r => !r.entry.isNewGuard)

      val (updatedForGuard, _) = currentGuardEntries.foldLeft((acc.getOrElse(currentGuard, Map()), LocalDateTime.now())) { (p, record) => record match {
        case Record(time, FallAsleep) => (p._1, time)
        case Record(time, WakeUp) =>
          val (map, start) = p
          val end = time

          ((0L until start.until(end, ChronoUnit.MINUTES)).foldLeft(map) { (map, delta) =>
            val minute = start.plusMinutes(delta).getMinute

            map + (minute -> (map.getOrElse(minute, 0) + 1))
          }, LocalDateTime.now())
      } }

      buildMinuteMap(rest, acc + (currentGuard -> updatedForGuard))
    }
  }

  private def parse(): Vector[Record] = {
    val wakesUp = "wakes up".r
    val fallAsleep = "falls asleep".r
    val beginShift = "Guard #(\\d+) begins shift".r

    input.map { line =>
      val parts = line.tail.split("] ")
      val time = LocalDateTime.parse(parts(0), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
      val entry = parts(1) match {
        case beginShift(id) => BeginShift(id.toInt)
        case wakesUp() => WakeUp
        case fallAsleep() => FallAsleep
      }

      Record(time, entry)
    }
  }

  case class Record(time: LocalDateTime, entry: Entry) extends Ordered[Record] {
    override def compare(that: Record): Int = if (time.isBefore(that.time)) -1
      else if (time.isAfter(that.time)) 1
      else 0
  }

  sealed trait Entry {
    val isNewGuard: Boolean
  }
  private case class BeginShift(id: Int) extends Entry {
    override val isNewGuard: Boolean = true
  }
  private case object WakeUp extends Entry {
    override val isNewGuard: Boolean = false
  }
  private case object FallAsleep extends Entry {
    override val isNewGuard: Boolean = false
  }
}

case object Day4 extends App {
  val input = Files.lines("2018/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
