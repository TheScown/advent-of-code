package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Problem, Timer}

case class Day16(input: String) extends Problem {
  override def solve1(): Unit = {
    val digits = input.toVector.map(_.asDigit)

    val result = (0 until 100).foldLeft(digits) { (digits, _) => computeTransform(digits) }
      .take(8)
      .mkString("")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val digits = (input * 10000).toVector.map(_.asDigit)
    val offset = digits.take(7).mkString("").toInt
    val relevantDigits = digits.drop(offset)

    val computed = (0 until 100).foldLeft(relevantDigits) { (digits, i) =>
      computeBigTransform(digits)
    }

    val result = computed
      .take(8)
      .mkString("")

    println(s"Result 2: $result")
  }

  private def computeTransform(input: Vector[Int]) = {
    input.indices.map { i =>
      val pattern = expandedPattern(i + 1).tail
      val result = input.zip(pattern).map { case (a, b) => a * b }.sum.abs % 10
      result
    }.toVector
  }

  private def computeBigTransform(input: Vector[Int]): Vector[Int] = {
    input.indices.reverse.tail.foldLeft(List(input.last)) { (acc, i) =>
      ((acc.head + input(i)) % 10) :: acc
    }.toVector
  }

  private def expandedPattern(position: Int): LazyList[Int] = {
    val basePattern = LazyList(0, 1, 0, -1)

    basePattern.flatMap(d => LazyList.fill(position)(d)) #::: expandedPattern(position)
  }
}

case object Day16 extends App {
  val input = Files.lines("2019/day16.txt").head
  val problem = Day16(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
