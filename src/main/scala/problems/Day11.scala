package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

case class Day11(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    var robotOutputVar: () => LazyList[(Long, Map[(Int, Int), Long])] = () => throw new IllegalStateException(s"Attempting to read too early")

    val computer = IntcodeComputer(program)
    val computerOutput = computer.execute(0.toLong #:: robotOutputVar().map(_._1)).map(_._2).filter(_.isDefined)

    val robot = Robot()
    val robotOutput = robot.run(computerOutput)
    robotOutputVar = () => robotOutput

    val finalState = robotOutputVar().last._2

    // Should be 2129
    println(s"Result 1: ${finalState.size}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    var robotOutputVar: () => LazyList[(Long, Map[(Int, Int), Long])] = () => throw new IllegalStateException(s"Attempting to read too early")

    val computer = IntcodeComputer(program)
    val computerOutput = computer.execute(1.toLong #:: robotOutputVar().map(_._1)).map(_._2).filter(_.isDefined)

    val robot = Robot()
    val robotOutput = robot.run(computerOutput)
    robotOutputVar = () => robotOutput

    val finalState = robotOutputVar().last._2

    // TODO printout the message represented by finalState
    println(s"Result 2: ${finalState.size}")
  }
}

case class Robot() {

  def run(input: LazyList[Option[Long]]): LazyList[(Long, Map[(Int, Int), Long])] = {
    def robot(currentPosition: (Int, Int), facing: Direction, mapping: Map[(Int, Int), Long], input: LazyList[Option[Long]], count: Int): LazyList[(Long, Map[(Int, Int), Long])] = {
      println(s"ROBOT: $currentPosition, $facing, $count")
      input match {
        case Some(nextColour) #:: Some(nextDirection) #:: remainingInput =>
//          println(s"$currentPosition")
          println(s"$nextColour,$nextDirection")
          val nextFacing = facing + nextDirection
          val nextPosition = nextFacing.nextPosition(currentPosition._1, currentPosition._2)
          (mapping.getOrElse(nextPosition, 0.toLong), mapping) #:: robot(nextPosition, nextFacing, mapping + (currentPosition -> nextColour), remainingInput, count + 1)
        case LazyList() =>
          println("Robot bailing out")
          LazyList((0, mapping))
        case remainingInput =>
          robot(currentPosition, facing, mapping, remainingInput, count)
      }
    }

    robot((0, 0), Up, Map(), input, 0)
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
    val value = Files.lines("day11.txt")
    Day11(value).solve1()
    Day11(value).solve2()
  }

}
