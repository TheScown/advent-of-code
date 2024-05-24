package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

import java.util.regex.Matcher

case class Day8(input: Vector[String]) extends Problem {

  private val hexPattern = "\\\\x[0-9a-f]{2}".r
  private val escapePattern = "\\\\(.)".r

  override def solve1(): Unit = {
    val result = input.map { line =>
      val codeSize = line.length
      val withoutHex = hexPattern.replaceAllIn(line, "x")
      val withoutEscapes = escapePattern.replaceAllIn(withoutHex, "$1")
      val memorySize = withoutEscapes.length - 2
      codeSize - memorySize
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = input.map { line =>
      val codeSize = line.length
      val escapeSlash = line.replaceAll("\\\\", Matcher.quoteReplacement("\\\\"))
      val escapeQuotes = escapeSlash.replaceAll("\"", Matcher.quoteReplacement("\\\""))
      val escapedSize = escapeQuotes.length + 2
      escapedSize - codeSize
    }.sum

    println(s"Result 2: $result")
  }
}

case object Day8 extends App {
  val input = Files.lines("2015/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
