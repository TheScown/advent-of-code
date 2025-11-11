package space.scown.adventofcode
package advent2020

import lib.{Files, Integers, Problem, Timer}

import scala.annotation.tailrec

case class Day25(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val cardPublicKey = input.head.toInt
    val doorPublicKey = input.last.toInt
    val mod = 20201227

    val cardLoopSize = findLoopSize(cardPublicKey, mod)
    val doorLoopSize = findLoopSize(doorPublicKey, mod)

    val encryptionKey1 = transform(cardLoopSize, doorPublicKey, mod)
    val encryptionKey2 = transform(doorLoopSize, cardPublicKey, mod)

    if (encryptionKey1 != encryptionKey2) {
      throw new IllegalStateException(s"Two different keys: $encryptionKey1, $encryptionKey2")
    }

    println(s"Result 1: $encryptionKey1")
  }

  override def solve2(): Unit = {
    println("Pay the deposit!")
  }

  private def findLoopSize(key: Int, mod: Int): Int = {
    @tailrec
    def helper(loopSize: Int, value: Long): Int = {
      val newValue = (value * 7L) % mod.toLong

      if (newValue.toInt == key) loopSize
      else helper(loopSize + 1, newValue)
    }

    helper(1, 1L)
  }

  private def transform(loopSize: Int, subjectNumber: Int, mod: Int): Int = {
    BigInt(subjectNumber).modPow(loopSize, mod).toInt
  }
}

case object Day25 extends App {
  val input = Files.lines("2020/day25.txt")
  val problem = Day25(input)
  Timer.time(() => problem.solve1())
  problem.solve2()
}
