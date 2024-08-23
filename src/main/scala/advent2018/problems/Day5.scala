package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day5(input: String) extends Problem {
  override def solve1(): Unit = {
    val result = react(input.toList)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = ('a' to 'z')
      .map(c => s"$c|${c.toUpper}")
      .map(pattern => input.replaceAll(pattern, ""))
      .map(test => react(test.toList))
      .min

    println(s"Result 2: $result")
  }

  @tailrec
  private def react(current: List[Char], acc: List[Char] = Nil): Int = {
    current match {
      case Nil => acc.size
      case head :: tail =>
        val flipped = if (head.isUpper) head.toLower else head.toUpper

        if (acc.nonEmpty && acc.head == flipped) react(tail, acc.tail)
        else react(tail, head :: acc)
    }
  }
}

case object Day5 extends App {
  val input = Files.lines("2018/day5.txt").head
  val problem = Day5(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
