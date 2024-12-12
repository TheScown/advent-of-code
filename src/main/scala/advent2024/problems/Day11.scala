package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem, Timer}

case class Day11(input: String) extends Problem {
  override def solve1(): Unit = {
    val values = input.split(" ").map(_.toLong).toVector

    val result = (1 to 25).foldLeft(values) { (values, _) =>
      values.flatMap(next)
    }.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val values = input.split(" ").map(x => (x.toLong, 75)).toVector

    def score(values: Vector[(Long, Int)], cache: Map[(Long, Int), Long]): (Long, Map[(Long, Int), Long]) = {
      values.foldLeft((0L, cache)) { case ((total, cache), value) =>
        if (value._2 == 0) (total + 1, cache + (value -> 1))
        else if (cache.contains(value)) (total + cache(value), cache)
        else {
          val nexts = next(value._1).map(x => (x, value._2 - 1))

          val (finalScore, updatedCache) = score(nexts, cache)

          (total + finalScore, updatedCache + (value -> finalScore))
        }
      }
    }

    val (result, _) = score(values, Map())

    println(s"Result 2: $result")
  }

  private def next(x: Long): Vector[Long] = {
    if (x == 0L) Vector(1L)
    else {
      val string = x.toString

      if (string.length % 2 == 0) {
        Vector(
          string.substring(0, string.length / 2).toLong,
          string.substring(string.length / 2).toLong,
        )
      }
      else Vector(x * 2024L)
    }
  }
}

case object Day11 extends App {
  val input = Files.lines("2024/day11.txt").head
  val problem = Day11(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
