package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Complex, Files, Grid, Problem}

case class Day18(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val blocks = parse()
    val grid = Grid.of[Boolean](71, 71, false)

    val updatedGrid = blocks.take(1024).foldLeft(grid) { (grid, block) =>
      grid.updated(block, true)
    }

    val finalState = bfs(updatedGrid)

    val result = finalState.get.steps

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val blocks = parse()
    val grid = Grid.of[Boolean](71, 71, false)

    val result = blocks.zipWithIndex.find { case (_, i) =>
      val updatedGrid = blocks.take(i + 1).foldLeft(grid) { (grid, block) =>
        grid.updated(block, true)
      }

      val finalState = bfs(updatedGrid)

      finalState.isEmpty
    }.map(p => s"${p._1.re},${-p._1.im}").get

    println(s"Result 2: $result")
  }

  private def bfs(grid: Grid[Boolean]) = {
    BFS.solve(Complex.ZERO[Int])(_ == Complex(70, -70)) { (p, _) =>
      Seq(Complex.ONE[Int], Complex.I[Int], -Complex.ONE[Int], -Complex.I[Int])
        .map(d => grid.next(p, d))
        .filterNot(n => p == n)
        .filterNot(n => grid(n))
    }
  }

  private def parse(): Vector[Complex[Int]] = {
    input.map { line =>
      val parts = line.split(",").map(_.toInt)
      Complex(parts(0), -parts(1))
    }
  }
}

case object Day18 extends App {
  val input = Files.lines("2024/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  problem.solve2()
}
