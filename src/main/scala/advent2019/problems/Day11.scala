package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode._
import lib.{Files, Problem}

import java.awt.image.BufferedImage
import java.awt.{Color, FlowLayout, Image}
import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}
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

    // Should be 2129
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
    val minX = paintedKeys.minBy(_._1)._1
    val maxX = paintedKeys.maxBy(_._1)._1
    val minY = paintedKeys.minBy(_._2)._2
    val maxY = paintedKeys.maxBy(_._2)._2

    val width = maxX - minX + 1
    val height = maxY - minY + 1

    val translateX = -minX
    val translateY = -minY

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for {
      x <- minX to maxX
      y <- minY to maxY
    } {
      val colour = if (robotOutput.getOrElse((x, y), 0) == 1) Color.WHITE.getRGB else Color.BLACK.getRGB
      image.setRGB(x + translateX, height - 1 - (y + translateY), colour)
    }

    renderImage(width, height, image)
  }

  private def renderImage(width: Int, height: Int, image: BufferedImage): Unit = {
    val frame = new JFrame()
    // Scale the image up so it's legible
    val icon = new ImageIcon(image.getScaledInstance(width * 10, height * 10, Image.SCALE_DEFAULT))
    frame.setLayout(new FlowLayout())
    // Need to allow height for the title bar
    frame.setSize(width * 10, height * 10 + 50)
    val label = new JLabel()
    label.setIcon(icon)
    frame.add(label)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  }
}

case class Robot() {

  def run(output: Output): Map[(Int, Int), Long] = {
    @tailrec
    def robot(currentPosition: (Int, Int), facing: Direction, mapping: Map[(Int, Int), Long], output: Output, count: Int): Map[(Int, Int), Long] = {
      println(s"ROBOT: $currentPosition, $facing, $count")
      if (output.outputs.isEmpty) {
        println("Robot bailing out")
        mapping
      }
      else {
        output.outputs match {
          case Seq(nextColour, nextDirection) =>
            println(s"$nextColour,$nextDirection")
            val nextFacing = facing + nextDirection
            val nextPosition = nextFacing.nextPosition(currentPosition._1, currentPosition._2)
            output match {
              case RequiresInput(_, continue) =>
                val nextOutput = continue(mapping.getOrElse(nextPosition, 0.toLong))
                robot(nextPosition, nextFacing, mapping + (currentPosition -> nextColour), nextOutput, count + 1)
              case Termination(_, _) =>
                println("Robot bailing out")
                mapping
            }
        }
      }
    }

    robot((0, 0), Up, Map(), output, 0)
  }

}

sealed trait Direction {
  def +(x : Long): Direction

  def nextPosition(x: Int, y: Int): (Int, Int)
}
case object Up extends Direction {
  override def +(x: Long): Direction = {
    if (x == 0) Left
    else if (x == 1) Right
    else throw new IllegalStateException(s"Invalid turn $x")
  }

  override def nextPosition(x: Int, y: Int): (Int, Int) = (x, y + 1)
}

case object Left extends Direction  {
  override def +(x: Long): Direction = {
    if (x == 0) Down
    else if (x == 1) Up
    else throw new IllegalStateException(s"Invalid turn $x")
  }

  override def nextPosition(x: Int, y: Int): (Int, Int) = (x - 1, y)
}

case object Right extends Direction  {
  override def +(x: Long): Direction = {
    if (x == 0) Up
    else if (x == 1) Down
    else throw new IllegalStateException(s"Invalid turn $x")
  }

  override def nextPosition(x: Int, y: Int): (Int, Int) = (x + 1, y)
}

case object Down extends Direction  {
  override def +(x: Long): Direction = {
    if (x == 0) Right
    else if (x == 1) Left
    else throw new IllegalStateException(s"Invalid turn $x")
  }

  override def nextPosition(x: Int, y: Int): (Int, Int) = (x, y - 1)
}

object Day11 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2019/day11.txt")
    Day11(value).solve1()
    Day11(value).solve2()
  }

}
