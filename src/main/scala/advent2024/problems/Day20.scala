package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Complex, Files, Grid, Problem, Timer}

case class Day20(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('S').get
    val end = grid.indexOf('E').get

    val (path, pathIndex) = getPath(start, end, grid)

    val result = path.map { position =>
      val cheats = grid.neighbours(position).filter(grid(_) == '#')
      val cheatExits = cheats.flatMap { cheat =>
        grid.neighbours(cheat)
          .filter(c => grid(c) != '#' && c != position)
      }

      cheatExits.map { c =>
        pathIndex(c) - pathIndex(position) - 2
      }.count(_ >= 100)
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('S').get
    val end = grid.indexOf('E').get

    val (path, pathIndex) = getPath(start, end, grid)

    val result = path.map { position =>
      val cheatExits = grid.indices
        .filter(i => (i mh position) <= 20)
        .filter(grid(_) != '#')

      cheatExits.map { c =>
        pathIndex(c) - pathIndex(position) - (c mh position)
      }.count(_ >= 100)
    }.sum

    println(s"Result 2: $result")
  }

  private def getPath(start: Complex[Int], end: Complex[Int], grid: Grid[Char]): (Vector[Complex[Int]], Map[Complex[Int], Int]) = {
    val initialState = PathState(start, Vector())
    val finalState = BFS.solve(initialState)(_.position == end) { (state, _) =>
      state match {
        case PathState(position, path) =>
          grid.neighbours(position)
            .filter(grid(_) != '#')
            .map(p => PathState(p, path :+ position))
      }
    }.get

    val path = finalState.value.path :+ finalState.value.position

    val pathIndex = path.zipWithIndex.toMap
    (path, pathIndex)
  }

  case class PathState(position: Complex[Int], path: Vector[Complex[Int]]) {
    override def hashCode(): Int = position.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case PathState(p, _) => p == position
      case _ => false
    }
  }
}

case object Day20 extends App {
  val input = Files.lines("2024/day20.txt")
  val problem = Day20(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
