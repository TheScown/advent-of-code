package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode._
import lib.{Complex, Files, Gui, Problem}

import java.awt.Color
import java.awt.image.BufferedImage
import scala.annotation.tailrec

case class Day11(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    val computer = IntcodeComputer(program)
    val computerOutput = computer.execute() match {
      case RequiresInput(_, continue) => continue(0)
    }

    val robot = Robot()
    val robotOutput = robot.run(computerOutput)

    println(s"Result 1: ${robotOutput.size}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    val computer = IntcodeComputer(program)
    val computerOutput = computer.execute() match {
      case RequiresInput(_, continue) => continue(1)
    }

    val robot = Robot()
    val robotOutput = robot.run(computerOutput)

    val paintedKeys = robotOutput.keySet
    val minX = paintedKeys.minBy(_.re).re
    val maxX = paintedKeys.maxBy(_.re).re
    val minY = paintedKeys.minBy(_.im).im
    val maxY = paintedKeys.maxBy(_.im).im

    val width = maxX - minX + 1
    val height = maxY - minY + 1

    val translateX = -minX
    val translateY = -minY

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for {
      x <- minX to maxX
      y <- minY to maxY
    } {
      val colour = if (robotOutput.getOrElse(Complex(x, y), 0) == 1) Color.WHITE.getRGB else Color.BLACK.getRGB
      image.setRGB(x + translateX, height - 1 - (y + translateY), colour)
    }

    Gui.renderImage(image)
  }

  case class Robot() {
    def run(output: Output): Map[Complex[Int], Long] = {
      @tailrec
      def robot(currentPosition: Complex[Int], facing: Complex[Int], mapping: Map[Complex[Int], Long], output: Output, count: Int): Map[Complex[Int], Long] = {
        if (output.outputs.isEmpty) {
          mapping
        }
        else {
          output.outputs match {
            case Seq(nextColour, nextDirection) =>
              val nextFacing = facing * (if (nextDirection == 0) Complex.I[Int] else -Complex.I[Int])
              val nextPosition = currentPosition + nextFacing
              output match {
                case RequiresInput(_, continue) =>
                  val nextOutput = continue(mapping.getOrElse(nextPosition, 0.toLong))
                  robot(nextPosition, nextFacing, mapping + (currentPosition -> nextColour), nextOutput, count + 1)
                case Termination(_, _) => mapping
              }
          }
        }
      }

      robot(Complex.ZERO, Complex.I, Map(), output, 0)
    }
  }
}

object Day11 extends App {
  val value = Files.lines("2019/day11.txt")
  val problem: Day11 = Day11(value)
  problem.solve1()
  problem.solve2()
}
