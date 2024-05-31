package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

case class Day13(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val inputMap = parse()
    val result = maxFromMap(inputMap)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val inputMap = parse().transform((_, v) => v + ("Self" -> 0))
    val names = inputMap.keys
    val mapWithSelf = inputMap + ("Self" -> names.map(name => name -> 0).toMap)
    val result = maxFromMap(mapWithSelf)

    println(s"Result 2: $result")
  }

  private def maxFromMap(map: Map[String, Map[String, Int]]) = {
    val namesWithSelf = map.keys.toVector
    val permutations = namesWithSelf.permutations

    val result = permutations.map { p =>
      p.appended(p.head).sliding(2).map {
        case Vector(a, b) => map(a)(b) + map(b)(a)
      }.sum
    }.max
    result
  }

  def parse(): Map[String, Map[String, Int]] = {
    val pattern = "([A-Z][a-z]+) would (gain|lose) (\\d+)[^A-Z]+([A-Z][a-z]+)\\.".r
    val tuples = input.map {
      case pattern(from, sign, value, to) =>
        val multiplier = if (sign == "gain") 1 else -1
        (from, (to, multiplier * value.toInt))
    }

    tuples.groupBy(_._1).transform((_, v) => Map.from(v.map(_._2)))
  }
}

case object Day13 extends App {
  val input = Files.lines("2015/day13.txt")
  val problem = new Day13(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
