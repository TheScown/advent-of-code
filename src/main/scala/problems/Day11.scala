package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

case class Day11(grid: Vector[Vector[Char]]) extends Problem {
  override def solve1(): Unit = {
    val result = solve(2)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = solve(1_000_000)

    println(s"Result 2: $result")
  }

  private def solve(expansionFactor: Long): Long = {
    val galaxyLocations = grid.zipWithIndex.flatMap {
      case (line, row) =>
        line.zipWithIndex.filter({
            case (c, _) => c == '#'
          })
          .map(_._2)
          .map(column => (row, column))
    }.sorted

    val emptyRows = grid.zipWithIndex.filter {
        case (line, _) => line.forall(_ == '.')
      }
      .map(_._2).toSet

    val emptyColumns = grid(0).indices.filter { column =>
      grid.indices.forall(row => grid(row)(column) == '.')
    }.toSet

    val distances = for {
      g1 <- galaxyLocations.indices
      g2 <- (g1 + 1) until galaxyLocations.size
    } yield {
      val (r1, c1) = galaxyLocations(g1)
      val (r2, c2) = galaxyLocations(g2)

      val smallR = Math.min(r1, r2)
      val largeR = Math.max(r1, r2)
      val smallC = Math.min(c1, c2)
      val largeC = Math.max(c1, c2)

      val unexpandedDistance = largeR - smallR + largeC - smallC

      val extraRows = (smallR until largeR).toSet.intersect(emptyRows).size * (expansionFactor - 1)
      val extraColumns = (smallC until largeC).toSet.intersect(emptyColumns).size * (expansionFactor - 1)

      val finalDistance = unexpandedDistance + extraRows + extraColumns

      (galaxyLocations(g1), galaxyLocations(g2), finalDistance)
    }

    distances.map(_._3).sum
  }
}

object Day11 {
  def main(args: Array[String]): Unit = {
    val value = Files.grid("day11.txt")
    time(Day11(value).solve1)
    time(Day11(value).solve2)
  }

}
