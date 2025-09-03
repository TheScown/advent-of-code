package space.scown.adventofcode
package advent2020

import lib.{Files, Integers, Problem}

case class Day13(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val startTime = input.head.toInt
    val timetable = input.last
    val busTimes = timetable
      .split(",")
      .filterNot(_ == "x")
      .map(_.toInt)
      .toVector

    val (nextBusId, nextBusWait) = busTimes.map { id =>
      val timeSinceBus = startTime % id
      val timeToNextBus = id - timeSinceBus
      (id, timeToNextBus)
    }.minBy(_._2)

    val result = nextBusId * nextBusWait

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val timetable = input.last
    val indexesWithIds = timetable
      .split(",")
      .toVector
      .zipWithIndex
      .filter(_._1 != "x")
      .map {
        case (id, index) => (-BigInt(index), BigInt(id))
      }

    val result = Integers.chineseRemainderTheorem(indexesWithIds)

    println(s"Result 2: $result")
  }
}

case object Day13 extends App {
  val input = Files.lines("2020/day13.txt")
  val problem = Day13(input)
  problem.solve1()
  problem.solve2()
}
