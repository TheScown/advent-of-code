package space.scown.adventofcode
package advent2016

import lib.{Crypto, Files, Integers, Problem, Timer}

import scala.annotation.tailrec
import scala.util.Random

case class Day5(input: String) extends Problem {
  override def solve1(): Unit = {
    val result = Integers.naturalNumbers
      .map(n => Crypto.md5(input + n))
      .filter(s => s.startsWith("00000"))
      .take(8)
      .toVector
      .map(_.charAt(5))
      .mkString("")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val characters = Integers.naturalNumbers
      .map(n => Crypto.md5(input + n))

    @tailrec
    def helper(state: State): Map[Int, Char] = {
      if (state.isFinished) state.found
      else helper(state.next)
    }

    val finalMap = helper(State(characters))
    val result = (0 until 8).map(finalMap).mkString("")

    println()
    println(s"Result 2: $result")
  }

  case class State(lazyList: LazyList[String], found: Map[Int, Char] = Map()) {
    private val range = 0 until 8
    private val unknown = "!@£$%^&*#"
    private val random = Random
    def isFinished: Boolean = range.forall(found.contains)

    def next: State = {
      @tailrec
      def helper(lazyList: LazyList[String]): ((Int, Char), LazyList[String]) = {
        val nextResultString = (0 until 8).map(i => found.getOrElse(i, unknown.charAt(random.nextInt(unknown.length)))).mkString("")

        print(s"\r$nextResultString")

        val nextString = lazyList.head
        val c = nextString.charAt(5).asDigit

        if (nextString.startsWith("00000") && range.contains(c) && !found.contains(c)) ((c, nextString.charAt(6)), lazyList.tail)
        else helper(lazyList.tail)
      }

      val (nextResult, newList) = helper(lazyList)

      State(newList, found + nextResult)
    }
  }
}

case object Day5 extends App {
  val input = Files.lines("2016/day5.txt").head
  val problem = Day5(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
