package space.scown.adventofcode
package advent2020

import lib.{Complex, Files, Problem, Timer}

import scala.annotation.tailrec

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val resultMap = instructions.foldLeft(Map[Complex[Int], Boolean]()) { (acc, line) =>
      val destination = line.sum

      acc + (destination -> !acc.getOrElse(destination, false))
    }

    val result = resultMap.values.count(_ == true)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    val initialMap = instructions.foldLeft(Map[Complex[Int], Boolean]()) { (acc, line) =>
      val destination = line.sum

      acc + (destination -> !acc.getOrElse(destination, false))
    }

    val finalMap = (0 until 100).foldLeft(initialMap) { (currentMap, _) =>
      val tilesToConsider = currentMap.keySet union currentMap.keySet.flatMap(neighbours)

      tilesToConsider.foldLeft(currentMap) { (updatedMap, address) =>
        val neighbouringTiles = neighbours(address)
        val blackNeighbours = neighbouringTiles.count(currentMap.getOrElse(_, false) == true)
        val isBlack = currentMap.getOrElse(address, false)

        if (isBlack) {
          updatedMap + (address -> !(blackNeighbours == 0 || blackNeighbours > 2))
        }
        else {
          updatedMap + (address -> (blackNeighbours == 2))
        }
      }.filter(_._2)
    }

    val result = finalMap.values.count(_ == true)

    println(s"Result 2: $result")
  }

  private def neighbours(address: Complex[Int]): Set[Complex[Int]] = {
    Set(
      address + Complex.ONE[Int],
      address - Complex.ONE[Int],
      address + Complex.I[Int],
      address - Complex.I[Int],
      address + Complex(1, 1),
      address - Complex(1, 1),
    )
  }

  private def parse(): Vector[Vector[Complex[Int]]] = {
    input.map { line =>
      @tailrec
      def helper(chars: Vector[Char], acc: Vector[Complex[Int]]): Vector[Complex[Int]] = {
        if (chars.isEmpty) acc
        else {
          chars match {
            // Eistenstein integers (i = ⍵)
            case 'e' +: rest => helper(rest, acc :+ Complex.ONE)
            case 'w' +: rest => helper(rest, acc :+ -Complex.ONE[Int])
            case 'n' +: 'w' +: rest => helper(rest, acc :+ Complex.I)
            case 's' +: 'e' +: rest => helper(rest, acc :+ -Complex.I[Int])
            case 'n' +: 'e' +: rest => helper(rest, acc :+ Complex(1, 1))
            case 's' +: 'w' +: rest => helper(rest, acc :+ -Complex(1, 1))
          }
        }
      }

      helper(line.toVector, Vector())
    }
  }
}

case object Day24 extends App {
  val input = Files.lines("2020/day24.txt")
  val problem = Day24(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
