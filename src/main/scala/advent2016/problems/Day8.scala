package space.scown.adventofcode
package advent2016.problems

import lib.{Complex, Files, Grid, Gui, Problem}

import java.awt.Color
import java.awt.image.BufferedImage
import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day8(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val finalGrid = generateDisplay

    val result = finalGrid.count(identity)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val finalGrid = generateDisplay

    val image = finalGrid.zipWithIndex.foldLeft(new BufferedImage(50, 6, BufferedImage.TYPE_INT_RGB)) { (image, item) =>
      val (value, index) = item
      val colour = if (value) Color.BLACK else Color.WHITE
      image.setRGB(index.re.toInt, -index.im.toInt, colour.getRGB)

      image
    }

    Gui.renderImage(image)
  }

  private def generateDisplay = {
    val instructions = parse()

    val initialGrid = Grid.of(6, 50, false)

    val finalGrid = instructions.foldLeft(initialGrid) { (grid, instruction) =>
      instruction match {
        case Rect(rows, columns) =>
          val rectGrid = Grid(Vector.fill(rows)(Vector.fill(columns)(true)))
          grid.updated(Complex.ZERO, rectGrid)
        case RotateRow(row, by) =>
          grid.rotateRow(row, by)
        case RotateColumn(column, by) =>
          grid.rotateColumn(column, -by)
      }
    }

    finalGrid
  }

  def parse(): Vector[Instruction] = {
    input.map {
      line => Grammar.parse(Grammar.instruction, line) match {
        case Grammar.Success(instruction, _) => instruction
        case error@Grammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
        case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
      }
    }
  }

  object Grammar extends RegexParsers {
    def instruction: Parser[Instruction] = rect | rotateRow | rotateColumn

    private def rect: Parser[Rect] = ("rect" ~> (number ~ ("x" ~> number))) ^^ {
      case columns ~ rows => Rect(rows.toInt, columns.toInt)
    }
    private def rotateRow: Parser[RotateRow] = ("rotate row y=" ~> number ~ ("by" ~> number)) ^^ {
      case row ~ by => RotateRow(row.toInt, by.toInt)
    }
    private def rotateColumn: Parser[RotateColumn] = ("rotate column x=" ~> number ~ ("by" ~> number)) ^^ {
      case column ~ by => RotateColumn(column.toInt, by.toInt)
    }

    private def number = "\\d+".r
  }

  trait Instruction
  private case class Rect(rows: Int, columns: Int) extends Instruction
  private case class RotateRow(row: Int, by: Int) extends Instruction
  private case class RotateColumn(column: Int, by: Int) extends Instruction
}

case object Day8 extends App {
  val input = Files.lines("2016/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
