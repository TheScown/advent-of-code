package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val digits = input.head.length
    val values = parse()

    val gammaBits = (0 until digits).map { i =>
      val mask = 1 << i
      val bits = values.map(b => (b & mask) >> i)
      val ones = bits.count(_ == 1)

      if (ones > input.size / 2) 1
      else 0
    }.reverse

    val gamma = Integer.parseInt(gammaBits.mkString(""), 2)

    val epsilonBits = gammaBits.map(b => if (b == 1) 0 else 1)
    val epsilon = Integer.parseInt(epsilonBits.mkString(""), 2)

    val result = gamma * epsilon

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val digits = input.head.length
    val values = parse()

    def countOnes(values: Vector[Int], i: Int): Int = {
      val shift = digits - 1 - i
      val mask = 1 << shift
      val bits = values.map(v => (v & mask) >> shift)
      bits.count(_ == 1)
    }

    val result = Seq(
      (oneCount: Int, zeroCount: Int) => if ((oneCount == zeroCount) || (oneCount > zeroCount)) 1 else 0,
      (oneCount: Int, zeroCount: Int) => if ((oneCount == zeroCount) || (zeroCount < oneCount)) 0 else 1,
    ).map { f =>
      (0 until digits).foldLeft(values) { (values, i) =>
        if (values.size == 1) values
        else {
          val oneCount = countOnes(values, i)
          val zeroCount = values.size - oneCount
          val targetDigit = f(oneCount, zeroCount)
          val shift = digits - 1 - i
          val mask = 1 << shift

          val newValues = values.filter(v => ((v & mask) >> shift) == targetDigit)
          newValues
        }
      }.head
    }.product

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Int] = {
    input.map(line => Integer.parseInt(line, 2))
  }
}

case object Day3 extends App {
  val input = Files.lines("2021/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
