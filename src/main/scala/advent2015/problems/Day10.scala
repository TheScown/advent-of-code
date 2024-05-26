package space.scown.adventofcode
package advent2015.problems

import advent2015.problems.Day10.{Element, elementsByName, elementsBySequence}
import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day10(input: String) extends Problem {
  override def solve1(): Unit = {
    val startElement = elementsBySequence(input)

    val result = helper(Seq(startElement), 40).map(_.sequence.length).sum
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val startElement = elementsBySequence(input)

    val result = helper(Seq(startElement), 50).map(_.sequence.length).sum
    println(s"Result 2: $result")
  }

  @tailrec
  private def helper(input: Seq[Day10.Element], remainingIterations: Int): Seq[Element] = {
    if (remainingIterations == 0) input
    else {
      val nextElements = input.flatMap {
        e => e.decaysInto.map(id => elementsByName(id))
      }

      helper(nextElements, remainingIterations - 1)
    }
  }
}

case object Day10 {

  private val elements = parseData
  private val elementsByName = elements.map(e => e.name -> e).toMap
  val elementsBySequence: Map[String, Element] = elements.map(e => e.sequence -> e).toMap

  def main(args: Array[String]): Unit = {
    val input = Files.lines("2015/day10.txt")(0)
    val problem = Day10(input)
    Timer.time(() => problem.solve1())
    Timer.time(() => problem.solve2())
  }

  private def parseData = {
    val pattern = "\\d+\t([A-Za-z]+)\t(\\d+)\t([A-Za-z]+(?:.[A-Za-z]+)*).*".r
    Files.lines("2015/look-and-say-elements.tsv").map {
      case pattern(element, sequence, decayPattern) =>
        Element(element, sequence, decayPattern.split("\\."))
    }
  }

  case class Element(name: String, sequence: String, decaysInto: Seq[String])
}
