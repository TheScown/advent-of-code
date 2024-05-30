package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day11(input: String) extends Problem {

  private val bannedChars = Set('i', 'o', 'l')

  override def solve1(): Unit = {
    @tailrec
    def helper(password: String): String = {
      if (isValidPassword(password)) password
      else helper(increment(password))
    }

    val result = helper(increment(input))

    // Should be vzbxxyzz
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    @tailrec
    def helper(password: String): String = {
      if (isValidPassword(password)) password
      else helper(increment(password))
    }

    val result1 = helper(increment(input))
    val result2 = helper(increment(result1))

    // Should be vzcaabcc
    println(s"Result 2: $result2")
  }

  private def isValidPassword(s: String): Boolean = {
    val hasRun = ('a' to 'z').exists { start =>
      val run = start.toString + (start + 1).toChar.toString + (start + 2).toChar.toString
      s.contains(run)
    }

    val containsBadLetter = bannedChars.exists(s.contains(_))

    val containsRepeatLetter = "([a-z])\\1".r.findAllMatchIn(s).size >=2

    !containsBadLetter && hasRun && containsRepeatLetter
  }

  private def increment(s: String): String = {
    @tailrec
    def helper(remainder: StringBuilder, acc: StringBuilder): StringBuilder = {
      val last = remainder.last
      if (last != 'z') {
        val nextChar = (last.toInt + 1).toChar

        if (bannedChars.contains(nextChar)) {
          val nextNextChar = (nextChar.toInt + 1).toChar

          remainder.deleteCharAt(remainder.length() - 1)
            .append(nextNextChar)
            .append("a".repeat(acc.length()))
        }
        else remainder.deleteCharAt(remainder.length() - 1)
          .append(nextChar)
          .append(acc)
      }
      else if (remainder.length() == 1) {
        throw new IllegalStateException("Trying to increment leading z")
      }
      else {
        helper(remainder.deleteCharAt(remainder.length() - 1), new StringBuilder("a").append(acc))
      }
    }

    helper(new StringBuilder(s), new StringBuilder()).toString()
  }

}

case object Day11 extends App {
  val input = Files.lines("2015/day11.txt")(0)
  val problem = new Day11(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
