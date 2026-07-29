package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day10(input: Vector[String]) extends Problem {

  private val pairing = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  override def solve1(): Unit = {
    val firstBadChars = parseLine(input).filter(_._2.isDefined).map(_._2.get)

    val result = firstBadChars.map {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val incompleteStacks = parseLine(input).filter(_._2.isEmpty).map(_._1)

    val completions = incompleteStacks.map { stack =>
      stack.foldLeft(Vector[Char]()) { (acc, c) =>
        val closingChar = pairing(c)
        acc :+ closingChar
      }
    }

    val scores = completions.map { completion =>
      completion.foldLeft(0L) { (score, c) =>
        5 * score + (c match {
          case ')' => 1
          case ']' => 2
          case '}' => 3
          case '>' => 4
        })
      }
    }

    val sortedScores = scores.sorted
    val result = sortedScores(scores.size / 2)

    println(s"Result 2: $result")
  }

  private def parseLine(input: Vector[String]): Vector[(List[Char], Option[Char])] = {
    input.map { line =>
      @tailrec
      def helper(stack: List[Char], remainingLine: Vector[Char]): (List[Char], Option[Char]) = {
        remainingLine match {
          case Vector() => (stack, None)
          case c +: rest =>
            if (pairing.contains(c)) {
              helper(c :: stack, rest)
            }
            else stack match {
              case Nil => (stack, Some(c))
              case head :: tail =>
                val expectedChar = pairing(head)

                if (c == expectedChar) helper(tail, rest)
                else (stack, Some(c))
            }
        }
      }

      helper(Nil, line.toVector)
    }
  }
}

case object Day10 extends App {
  val input = Files.lines("2021/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
