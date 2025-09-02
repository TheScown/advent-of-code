package space.scown.adventofcode
package advent2020

import lib.{Complex, Files, Problem}

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructionPattern = "([A-Z])(\\d+)".r
    val instructions = input.map {
      case instructionPattern(operation, value) => (operation, value.toInt)
    }

    val initialPosition = Complex.ZERO[Int]

    val (finalPosition, _) = instructions.foldLeft((initialPosition, Complex.ONE[Int])) { case ((position, direction), (operation, value)) =>
      operation match {
        case "N" => (position + Complex(0, value), direction)
        case "E" => (position + Complex(value, 0), direction)
        case "S" => (position + Complex(0, -value), direction)
        case "W" => (position + Complex(-value, 0), direction)

        case "L" =>
          val directionChange = value match {
            case 0 => Complex.ONE[Int]
            case 90 => Complex.I[Int]
            case 180 => -Complex.ONE[Int]
            case 270 => -Complex.I[Int]
          }

          (position, direction * directionChange)
        case "R" =>
          val directionChange = value match {
            case 0 => Complex.ONE[Int]
            case 90 => -Complex.I[Int]
            case 180 => -Complex.ONE[Int]
            case 270 => Complex.I[Int]
          }

          (position, direction * directionChange)

        case "F" => (position + (direction * value), direction)
      }
    }

    val result = finalPosition.mh(initialPosition)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructionPattern = "([A-Z])(\\d+)".r
    val instructions = input.map {
      case instructionPattern(operation, value) => (operation, value.toInt)
    }

    val initialPosition = Complex.ZERO[Int]
    val initialWaypoint = Complex(10, 1)

    val (finalPosition, _) = instructions.foldLeft((initialPosition, initialWaypoint)) { case ((position, waypoint), (operation, value)) =>
      operation match {
        case "N" => (position, waypoint + Complex(0, value))
        case "E" => (position, waypoint + Complex(value, 0))
        case "S" => (position, waypoint + Complex(0, -value))
        case "W" => (position, waypoint + Complex(-value, 0))

        case "L" =>
          val directionChange = value match {
            case 0 => Complex.ONE[Int]
            case 90 => Complex.I[Int]
            case 180 => -Complex.ONE[Int]
            case 270 => -Complex.I[Int]
          }

          (position, waypoint * directionChange)
        case "R" =>
          val directionChange = value match {
            case 0 => Complex.ONE[Int]
            case 90 => -Complex.I[Int]
            case 180 => -Complex.ONE[Int]
            case 270 => Complex.I[Int]
          }

          (position, waypoint * directionChange)

        case "F" => (position + (waypoint * value), waypoint)
      }
    }

    val result = finalPosition.mh(initialPosition)

    println(s"Result 2: $result")
  }
}

case object Day12 extends App {
  val input = Files.lines("2020/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}
