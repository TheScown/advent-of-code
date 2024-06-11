package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Integers, Problem, Timer}

import scala.annotation.tailrec

case class Day20(input: String) extends Problem {

  override def solve1(): Unit = {
    val target = input.toInt / 10
    val result = Integers.naturalNumbers.find { n =>
      Integers.factors(n).sum > target
    }.get

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val target = input.toInt / 11
    val initialAvailability = 50

    @tailrec
    def helper(numbers: LazyList[Int], available: Map[Int, Int]): Int = numbers match {
      case n #:: rest =>
        val factors = Integers.factors(n)
          .filter(d => available.getOrElse(d, initialAvailability) > 0)

        val factorSum = factors.sum

        if (factorSum > target) n
        else {
          val updatedAvailability = factors.foldLeft(available) { (av, d) =>
            av + (d -> (av.getOrElse(d, initialAvailability) - 1))
          }

          helper(rest, updatedAvailability)
        }
    }

    val result = helper(Integers.naturalNumbers, Map())

    println(s"Result 2: $result")
  }
}

case object Day20 extends App {
  val input = Files.lines("2015/day20.txt")
  val problem = Day20(input(0))
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
