package space.scown.adventofcode
package advent2021

import lib.{Complex, Dijkstra, Files, Grid, Problem}

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.map(_.asDigit).toVector))

    val start = State(Complex.ZERO[Int], 0)

    val result: Int = solve(grid, start)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val baseGrid = Grid(input.map(_.map(_.asDigit).toVector))

    val additions = Grid.of(5, 5, false).zipWithIndex.map(_._2 mh Complex.ZERO[Int])

    val subGrids = additions.map { a =>
      baseGrid.map { x =>
        val newValue = x + a

        if (newValue > 9) newValue - 9 else newValue
      }
    }

    val finalGrid = Grid.flatten(subGrids)

    val start = State(Complex.ZERO[Int], 0)

    val result: Int = solve(finalGrid, start)

    println(s"Result 2: $result")
  }

  private def solve(grid: Grid[Int], start: State) = {
    val finalState = Dijkstra.solve(start)(
      Ordering.by[State, Int](_.score).reverse,
      _.address == Complex(grid.rowLength - 1, -grid.columnLength + 1)
    ) { case State(address, score) =>
      val neighbours = grid.neighbours(address)

      neighbours.map(n => State(n, score + grid(n)))
    }.get

    finalState.score
  }

  case class State(address: Complex[Int], score: Int) {
    override def equals(obj: Any): Boolean = obj match {
      case State(a, _) => a == address
      case _ => false
    }

    override def hashCode(): Int = address.hashCode()
  }
}

case object Day15 extends App {
  val input = Files.lines("2021/day15.txt")
  val problem = Day15(input)
  problem.solve1()
  problem.solve2()
}
