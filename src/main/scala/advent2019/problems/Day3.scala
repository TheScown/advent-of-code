package space.scown.adventofcode
package advent2019.problems

import lib.{Complex, Files, Problem}

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val wires = parse()

    val wireSets = wires.map { wire =>
      wire.foldLeft((Complex.ZERO[Int], Set[Complex[Int]]())) { case ((position, set), item) =>
        val direction = item.normalised
        val newPosition = position + item
        val range = Complex.linearRange(position, newPosition, direction, inclusive = true).toSet
        (newPosition, set union range)
      }
    }.map(_._2)

    val crossovers = wireSets.reduce(_ intersect _) - Complex.ZERO
    val result = crossovers.minBy(_ mh Complex.ZERO) mh Complex.ZERO

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val wires = parse()

    val wireMaps = wires.map { wire =>
      wire.foldLeft((Complex.ZERO[Int], 0, Map[Complex[Int], Int]())) { case ((position, steps, distanceMap), item) =>
        val direction = item.normalised
        val newPosition = position + item
        val range = Complex.linearRange(position, newPosition, direction, inclusive = true).zipWithIndex
        val updatedMap = range.foldLeft(distanceMap) { case (map, (position, index)) =>
          map + (position -> distanceMap.getOrElse(position, steps + index))
        }
        (newPosition, steps + range.size - 1, updatedMap)
      }
    }.map(_._3)

    val crossovers = wireMaps.map(_.keySet).reduce(_ intersect _) - Complex.ZERO
    val result = crossovers.map(position => wireMaps.map(_.apply(position)).sum).min

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Vector[Complex[Int]]] = {
    val pattern = "([RULD])(\\d+)".r

    input.map { line =>
      line.split(",").toVector.map {
        case pattern(direction, quantity) =>
          val unitNumber = direction match {
            case "R" => Complex.ONE[Int]
            case "U" => Complex.I[Int]
            case "L" => -Complex.ONE[Int]
            case "D" => -Complex.I[Int]
          }
          unitNumber * quantity.toInt
      }
    }
  }
}

case object Day3 extends App {
  val input = Files.lines("2019/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
