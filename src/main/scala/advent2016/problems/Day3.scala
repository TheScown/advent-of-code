package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val parsedInput = parse()

    val result = countTriangles(parsedInput)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val transposedInput = parse().transpose
    val newTriangles = transposedInput.flatMap { col =>
      col.grouped(3).toVector
    }

    val result = countTriangles(newTriangles)

    println(s"Result 2: $result")
  }

  private def countTriangles(parsedInput: Vector[Vector[Int]]) = {
    parsedInput.count { line =>
      val possibleArrangements = for {
        i <- line.indices
      } yield line.zipWithIndex.partition(p => p._2 == i)

      possibleArrangements.forall {
        case (head, tail) => head.head._1 < tail.map(_._1).sum
      }
    }
  }

  def parse(): Vector[Vector[Int]] = {
    input.map { line =>
      line.trim().split("\\s+").map(_.toInt).toVector
    }
  }
}

case object Day3 extends App {
  val input = Files.lines("2016/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}