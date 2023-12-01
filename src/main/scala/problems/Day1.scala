package space.scown.advent2023
package problems

import space.scown.advent2023.lib.{Files, Problem}

import scala.util.matching.Regex

case class Day1(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val patterns = Vector("\\d")
    val patternUnion = patterns.mkString("|").r

    val result = lines.map { line => {
      val firstDigit = matchLine(line, patternUnion)
      val lastDigit = matchLine(line.reverse, patternUnion)

      s"$firstDigit$lastDigit".toInt
    }}.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val digitLookup = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9",
      "zero" -> "0"
    )

    val staticPatterns = Vector("\\d")
    val patternsToReverse = digitLookup.keys.toVector

    val patterns = staticPatterns ++ patternsToReverse
    val patternUnion = patterns.mkString("|").r

    val reversedPatterns = staticPatterns ++ patternsToReverse.map { p => p.reverse }
    val reversedPatternUnion = reversedPatterns.mkString("|").r

    val result = lines.map { line => {
      val firstDigitString = matchLine(line, patternUnion)
      val lastDigitString = matchLine(line.reverse, reversedPatternUnion).reverse

      val firstDigit = digitLookup.getOrElse(firstDigitString, firstDigitString)
      val lastDigit = digitLookup.getOrElse(lastDigitString, lastDigitString)

      s"$firstDigit$lastDigit".toInt
    }
    }.sum

    println(s"Result 2: $result")
  }

  private def matchLine(line: String, pattern: Regex): String = {
    pattern.findFirstIn(line) match {
      case Some(s) => s
      case None => throw new IllegalArgumentException(s"Invalid line $line")
    }
  }
}

object Day1 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day1.txt")
    Day1(value).solve1()
    Day1(value).solve2()
  }

}