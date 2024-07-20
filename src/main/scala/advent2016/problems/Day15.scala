package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Integers, Problem, Timer}

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val discs = parse()

    val result = Integers.chineseRemainderTheorem(discs.map(d => (d.desiredPosition, d.positions)))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val discs = parse() :+ Disc(7, 11, 0)

    val result = Integers.chineseRemainderTheorem(discs.map(d => (d.desiredPosition, d.positions)))

    println(s"Result 2: $result")
  }

  def parse(): Vector[Disc] = {
    input.map { line =>
      val pattern = "\\d+".r
      val values = pattern.findAllMatchIn(line).map(m => BigInt(m.matched)).toVector
      Disc(values(0), values(1), values(3))
    }
  }

  case class Disc(distance: BigInt, positions: BigInt, offset: BigInt) {
    // The position of the disc at time t such that it will reach position = 0 at t + distance
    val desiredPosition: BigInt = {
      (-offset - distance) % positions
    }
  }
}

case object Day15 extends App {
  val input = Files.lines("2016/day15.txt")
  val problem = Day15(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
