package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

case class Day8(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val rows = parse()

    val result = rows.map {
      case (_, rhs) =>
        rhs.count(s => s.size == 2 || s.size == 3 || s.size == 4 || s.size == 7)
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val rows = parse()

    val result = rows.map {
      case (definitions, values) =>
        val segmentDefinitions = segments(definitions)

        values.map { value =>
          val positions = value.map(segmentDefinitions.apply)
          positionsToDigit(positions)
        }.mkString("").toInt
    }.sum

    println(s"Result 2: $result")
  }

  private def parse(): Vector[(Vector[Set[Char]], Vector[Set[Char]])] = {
    def sideToSets(side: String): Vector[Set[Char]] = {
      side.split(" ").toVector.map(_.toSet)
    }

    input.map { line =>
      line.split(" \\| ") match {
        case Array(lhs, rhs) =>
          (sideToSets(lhs), sideToSets(rhs))
      }
    }
  }

  private def positionsToDigit(positions: Set[Position]): Char = {
    if (positions == Set(TopRight, BottomRight)) '1'
    else if (positions == Set(Top, TopRight, Middle, BottomLeft, Bottom)) '2'
    else if (positions == Set(Top, TopRight, Middle, BottomRight, Bottom)) '3'
    else if (positions == Set(TopLeft, TopRight, Middle, BottomRight)) '4'
    else if (positions == Set(Top, TopLeft, Middle, BottomRight, Bottom)) '5'
    else if (positions == Set(Top, TopLeft, Middle, BottomLeft, BottomRight, Bottom)) '6'
    else if (positions == Set(Top, TopRight, BottomRight)) '7'
    else if (positions == Set(Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom)) '8'
    else if (positions == Set(Top, TopLeft, TopRight, Middle, BottomRight, Bottom)) '9'
    else if (positions == Set(Top, TopLeft, TopRight, BottomLeft, BottomRight, Bottom)) '0'
    else throw new IllegalStateException(s"Bad digit: $positions")
  }

  private def segments(definitions: Vector[Set[Char]]): Map[Char, Position] = {
    // The one digit has two segments, TopRight and BottomRight
    val one = definitions.find(d => d.size == 2).get

    // The seven digit has three segments, TopRight, BottomRight and Top
    val seven = definitions.find(d => d.size == 3).get

    // Top is the value in seven but not in one
    val top = (seven -- one).head

    // Four has four digits, TopRight, BottomRight, TopLeft and Middle
    val four = definitions.find(d => d.size == 4).get

    // TopLeft and Middle are in four but not in one
    val topLeftAndMiddle = four -- one

    // three has five segments, including all of one
    val three = definitions.find(d => d.size == 5 && one.subsetOf(d)).get

    // TopLeft is not in three, so the one that is must be middle
    val middle = (topLeftAndMiddle intersect three).head

    // TopLeft is not Middle
    val topLeft = (topLeftAndMiddle - middle).head

    // The remaining segment in three is Bottom
    val bottom = ((three -- one) - top - middle).head

    // five has five segments, including top left
    val five = definitions.find(d => d.size == 5 && d.contains(topLeft)).get

    // BottomRight is in both one and five
    val bottomRight = (one intersect five).head

    // TopRight is the other digit in one
    val topRight = (one - bottomRight).head

    // two has five segments, including top right but not bottom right
    val two = definitions.find(d => d.size == 5 && d.contains(topRight) && !d.contains(bottomRight)).get

    // BottomLeft is the only unidentified digit in two
    val bottomLeft = (two - top - middle - bottom - topRight).head

    Map(
      top -> Top,
      topLeft -> TopLeft,
      topRight -> TopRight,
      middle -> Middle,
      bottomLeft -> BottomLeft,
      bottomRight -> BottomRight,
      bottom -> Bottom
    )
  }

  private sealed trait Position
  private case object Top extends Position
  private case object TopLeft extends Position
  private case object TopRight extends Position
  private case object Middle extends Position
  private case object BottomLeft extends Position
  private case object BottomRight extends Position
  private case object Bottom extends Position
}

case object Day8 extends App {
  val input = Files.lines("2021/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
