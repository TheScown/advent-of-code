package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode._
import lib._

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

case class Day13(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program)
    val outputs = computer.execute().outputs
    val result = outputs.grouped(3).map(_.apply(2)).count(_ == 2L)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(input).updated(0, 2L)
    val computer = IntcodeComputer(program)
    val output = computer.execute()
    val outputs = output.outputs
    val tiles = outputs.map(_.toInt).grouped(3).toVector

    val minX = 0
    val maxX = tiles.map(_.head).max
    val minY = 0
    val maxY = tiles.map(_.apply(1)).max
    val rows = maxY - minY + 1
    val columns = maxX - minX + 1

    val grid = Grid.of(rows, columns, ' ')

    val panel = new GridPanel[Char](grid, 260, 370)
    val window = Gui.getPanelWindow(panel, 260, 370)

    val delay = 1L

    @tailrec
    def gameLoop(output: Output): Int = {
      TimeUnit.MILLISECONDS.sleep(delay)
      val tiles = output.outputs.map(_.toInt).grouped(3).toVector
      val newGrid = updateGrid(tiles, panel.grid)
      panel.setGrid(newGrid)

      output match {
        case RequiresInput(_, continue) =>
          val ballPosition = newGrid.indexOf('B').get
          val paddlePosition = newGrid.indexOf('P').get

          val nextInput = ballPosition.re.compareTo(paddlePosition.re).toLong
          gameLoop(continue(nextInput))
        case Termination(_, _) => tiles.find(_.head == -1).get.apply(2)
      }
    }

    val result = gameLoop(output)
    window.dispose()

    println(s"Result 2: $result")
  }

  private def updateGrid(tiles: Vector[Seq[Int]], grid: Grid[Char]): Grid[Char] = {
    tiles.foldLeft(grid) { (grid, tile) =>
      if (tile.head == -1) {
        grid
      }
      else grid.updated(Complex(tile.head, -tile(1)), tile(2) match {
        case 0 => ' '
        case 1 => '#'
        case 2 => 'X'
        case 3 => 'P'
        case 4 => 'B'
      })
    }
  }
}

case object Day13 extends App {
  val input = Files.lines("2019/day13.txt")
  val problem = Day13(input)
  problem.solve1()
  problem.solve2()
}
