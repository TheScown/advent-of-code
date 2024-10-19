package space.scown.adventofcode
package advent2018.problems

import lib.{Complex, Files, Problem, Timer}

import scala.language.postfixOps
import scala.math.Numeric.IntIsIntegral
import scala.util.parsing.combinator.RegexParsers

case class Day20(input: String) extends Problem {
  override def solve1(): Unit = {
    val graph = parse()

    val resultMap = buildMap(graph)
    val result = resultMap.values.max

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val graph = parse()

    val resultMap = buildMap(graph)
    val result = resultMap.values.count(_ >= 1000)

    println(s"Result 2: $result")
  }

  private def buildMap(graph: List[Node]) = {
    def helper(sequence: List[Node], position: Complex[Int], distances: Map[Complex[Int], Int]): (Map[Complex[Int], Int], Complex[Int]) = {
      if (sequence.isEmpty) (distances, position)
      else {
        sequence.head match {
          case Single(direction) =>
            val newPosition = position + direction

            if (distances.contains(newPosition)) helper(sequence.tail, newPosition, distances)
            else helper(sequence.tail, newPosition, distances + (newPosition -> (distances(position) + 1)))
          case Branch(paths) =>
            val distanceChoices = paths.map(path => helper(path, position, distances))
            val combinedChoices = distanceChoices.groupBy(_._2).map {
              case (position, maps) => (combineMaps(maps.map(_._1)), position)
            }.toList

            val finalMaps = combinedChoices.map {
              case (distances, position) => helper(sequence.tail, position, distances)
            }.map(_._1)

            (combineMaps(finalMaps), Complex.ZERO)
        }
      }
    }

    val (resultMap, _) = helper(graph, Complex.ZERO, Map() + (Complex.ZERO(IntIsIntegral) -> 0))
    resultMap
  }

  private def parse(): List[Node] = {
    Grammar.parse(Grammar.path, input) match {
      case Grammar.Success(result, _) => result
      case Grammar.NoSuccess.I(err, _) => throw new IllegalArgumentException(err)
    }
  }

  private def combineMaps(finalMaps: List[Map[Complex[Int], Int]]) = {
    finalMaps.foldLeft(Map[Complex[Int], Int]()) { (acc, distances) =>
      distances.foldLeft(acc) { case (acc, (position, distance)) =>
        if (acc.contains(position) && acc(position) < distance) acc
        else acc + (position -> distance)
      }
    }
  }

  case object Grammar extends RegexParsers {
    def path: Parser[List[Node]] = '^' ~> actualPath <~ '$'
    def actualPath: Parser[List[Node]] = (direction | parens)*
    def parens: Parser[Node] = "(" ~ repsep(actualPath, "|") ~ ")" ^^ {
      case _ ~ paths ~ _ => Branch(paths)
    }
    def direction: Parser[Node] = "[NEWS]".r ^^ (s => Single(s.head match {
      case 'N' => Complex.I
      case 'E' => Complex.ONE
      case 'S' => -Complex.I(IntIsIntegral)
      case 'W' => -Complex.ONE(IntIsIntegral)
    }))
  }

  trait Node
  case class Single(char: Complex[Int]) extends Node
  case class Branch(branches: List[List[Node]]) extends Node
}

case object Day20 extends App {
  val input = Files.lines("2018/day20.txt").head
  val problem = Day20(input)
  Timer.time(() => problem.solve1())
  problem.solve2()
}
