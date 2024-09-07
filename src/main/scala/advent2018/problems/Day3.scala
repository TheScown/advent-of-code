package space.scown.adventofcode
package advent2018.problems

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val claims = parse()
    val grid = Grid.of(1000, 1000, 0)

    val finalGrid = claims.foldLeft(grid) { (grid, claim) =>
      val slice = grid.slice(claim.position, claim.width, claim.height)
      val incremented = slice.map(_ + 1)
      grid.updated(claim.position, incremented)
    }

    val result = finalGrid.count(_ >= 2)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val claims = parse()
    val grid = Grid.of(1000, 1000, Set[Claim]())

    val finalGrid = claims.foldLeft(grid) { (grid, claim) =>
      val slice = grid.slice(claim.position, claim.width, claim.height)
      val incremented = slice.map(_ + claim)
      grid.updated(claim.position, incremented)
    }

    @tailrec
    def helper(remainingClaims: Vector[Claim]): Claim = {
      val next = remainingClaims.head

      val currentSlice = finalGrid.slice(next.position, next.width, next.height)
      val usedClaims = currentSlice.values.flatten.flatten.toSet

      if (usedClaims.size == 1) next
      else helper(remainingClaims.filterNot(usedClaims.contains))
    }

    val result = helper(claims).id

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Claim] = {
    val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

    input.map {
      case pattern(id, re, im, width, height) => Claim(id.toInt, Complex(re.toInt, -im.toInt), width.toInt, height.toInt)
    }
  }

  case class Claim(id: Int, position: Complex[Int], width: Int, height: Int)
}

case object Day3 extends App {
  val input = Files.lines("2018/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
