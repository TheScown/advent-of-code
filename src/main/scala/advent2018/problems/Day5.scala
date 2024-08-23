package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day5(input: String) extends Problem {
  private val pattern = ('a' to 'z').map(c => s"$c${c.toUpper}|${c.toUpper}$c").mkString("|")

  override def solve1(): Unit = {
    val result = react(input)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = ('a' to 'z')
      .map(c => s"$c|${c.toUpper}")
      .map(pattern => input.replaceAll(pattern, ""))
      .map(test => react(test))
      .min

    println(s"Result 2: $result")
  }

  @tailrec
  private def react(current: String, previous: String = ""): Int = {
    if (current == previous) current.length
    else react(current.replaceAll(pattern, ""), current)
  }
}

case object Day5 extends App {
  val input = Files.lines("2018/day5.txt").head
  val problem = Day5(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
