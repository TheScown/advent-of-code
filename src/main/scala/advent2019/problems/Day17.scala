package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{AsciiAdapter, IntcodeComputer, IntcodeProgram}
import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day17(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program)
    val asciiComputer = AsciiAdapter(computer.execute())
    val output = asciiComputer.stringOutput

    val grid = Grid(output.split("\\n").toVector.map(_.toVector))

    val result = grid.zipWithIndex.filter {
      case (c, i) => c == '#' && grid.neighbours(i).map(grid.apply).count(_ == '#') == 4
    }.map {
      case (_, Complex(re, im)) => (re * im).abs
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(input).updated(0, 2L)
    val computer = IntcodeComputer(program)
    val asciiComputer = AsciiAdapter(computer.execute())

    val output = asciiComputer.stringOutput
    val grid = Grid(output.split("\\n").takeWhile(_.nonEmpty).toVector.map(_.toVector))
    val path = computePath(grid)

    def findParts(path: String, parts: Vector[String]): Option[Vector[String]] = {
      if (parts.size == 3) {
        if (path.isEmpty) Some(parts)
        else None
      }
      else {
        (1 to 20)
          .map { i => path.substring(0, i) }
          .filter(s => !s.endsWith(","))
          .map { substring =>
            val replaced = path.replaceAll(substring + "(,|$)", "")
            findParts(replaced, parts :+ substring)
          }
          .find(_.isDefined) match {
            case Some(parts) => parts
            case None => None
          }
      }
    }

    val parts = findParts(path, Vector()).get

    val main = path.replaceAll(parts(0), "A").replaceAll(parts(1), "B").replaceAll(parts(2), "C")

    val continuousOutput = "n"

    val finalState = Seq(main, parts(0), parts(1), parts(2), continuousOutput).map(_ + "\n").foldLeft(asciiComputer) { (asciiComputer, string) =>
      asciiComputer.sendString(string)
    }

    val result = finalState.outputs.last

    println(s"Result 2: $result")
  }

  private def computePath(grid: Grid[Char]): String = {
    @tailrec
    def pathHelper(position: Complex[Int], direction: Complex[Int], stepCount: Int, path: Vector[String]): Vector[String] = {
      val next = grid.next(position, direction)

      if (next == position || grid(next) == '.') {
        val left = grid.next(position, direction * Complex.I[Int])
        val right = grid.next(position, direction * -Complex.I[Int])
        val gridLeft = grid(left)
        val gridRight = grid(right)

        val nextPath = if (stepCount == 0) path else path :+ stepCount.toString

        if (left != position && right != position && gridLeft == '.' && gridRight == '.') nextPath
        else if (left != position && gridLeft == '.') pathHelper(position, direction * -Complex.I[Int], 0, nextPath :+ "R")
        else pathHelper(position, direction * Complex.I[Int], 0, nextPath :+ "L")
      } else {
        pathHelper(next, direction, stepCount + 1, path)
      }
    }

    val initialPosition = grid.indexWhere(c => c != '.' && c != '#').get
    val initialDirection = grid(initialPosition) match {
      case '^' => Complex.I[Int]
      case 'v' => -Complex.I[Int]
      case '>' => Complex.ONE[Int]
      case '<' => -Complex.ONE[Int]
    }

    pathHelper(initialPosition, initialDirection, 0, Vector()).mkString(",")
  }
}

case object Day17 extends App {
  val input = Files.lines("2019/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
