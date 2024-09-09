package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Integers, Problem, Timer}

case class Day13(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val pairs = parse()

    val result = severity(triggeredLayers(pairs))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val pairs = parse()

    val result = Integers.naturalNumbers[Int].find(n => triggeredLayers(pairs, n).isEmpty).get

    println(s"Result 2: $result")
  }

  private def severity(triggeredLayers: Vector[(Int, Int)]): Int = {
    triggeredLayers.map(p => p._1 * p._2).sum
  }

  private def triggeredLayers(pairs: Vector[(Int, Int)], delay: Int = 0): Vector[(Int, Int)] = {
    pairs.filter {
      case (layer, range) => (layer + delay) % (2 * (range - 1)) == 0
    }
  }

  private def parse(): Vector[(Int, Int)] = {
    input
      .map(line => line.split(":").map(_.trim.toInt))
      .map(parts => (parts(0), parts(1)))
  }
}

case object Day13 extends App {
  val input = Files.lines("2017/day13.txt")
  val problem = Day13(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
