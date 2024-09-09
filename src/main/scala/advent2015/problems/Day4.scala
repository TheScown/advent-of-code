package space.scown.adventofcode
package advent2015.problems

import lib.Crypto.md5
import lib.Integers.naturalNumbers
import lib.{Files, Problem, Timer}

import scala.util.matching.Regex

case class Day4(input: String) extends Problem {
  override def solve1(): Unit = {
    val result: Int = findLeadingZeros("^0{5}".r)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result: Int = findLeadingZeros("^0{6}".r)

    println(s"Result 2: $result")
  }

  private def findLeadingZeros(pattern: Regex) = {
    val result = naturalNumbers[Int].iterator.find { n =>
      val s = input + n.toString
      val hash = md5(s)
      pattern.findFirstMatchIn(hash).isDefined
    }.get
    result
  }
}

case object Day4 extends App {
  val input = Files.lines("2015/day4.txt")(0)
  val problem = Day4(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
