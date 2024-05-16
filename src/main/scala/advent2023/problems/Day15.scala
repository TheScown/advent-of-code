package space.scown.adventofcode
package advent2023.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day15(lines: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val input = lines.mkString("").split(",").toVector

    val result = input.map(hash).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = lines.mkString("").split(",").toVector

    val boxes = Vector.fill(256)(Vector[Lens]())

    val instructionPattern = "([^=-]+)([-=])(\\d*)".r

    @tailrec
    def buildBoxes(instructions: Vector[String], boxes: Vector[Vector[Lens]]): Vector[Vector[Lens]] = {
      if (instructions.isEmpty) boxes
      else {
        val instruction = instructions.head
        val regexMatch = instructionPattern.findFirstMatchIn(instruction).get
        val label = regexMatch.group(1)
        val operator = regexMatch.group(2)
        val focalLength = if (operator == "=") regexMatch.group(3).toInt else -1

        val boxIndex = hash(label)
        val box = boxes(boxIndex)

        val updatedBoxes = operator match {
          case "=" =>
            val updatedBox = box.indexWhere(lens => lens.label == label) match {
              case -1 => box.appended(Lens(label, focalLength))
              case i => box.updated(i, Lens(label, focalLength))
            }
            boxes.updated(boxIndex, updatedBox)
          case "-" =>
            val updatedBox = box.filterNot(lens => lens.label == label)
            boxes.updated(boxIndex, updatedBox)
        }

        buildBoxes(instructions.tail, updatedBoxes)
      }
    }

    val finalBoxes = buildBoxes(instructions, boxes)

    val result = finalBoxes.zipWithIndex.map { case (box, boxIndex) =>
      (boxIndex + 1) * box.zipWithIndex.map { case (lens, index) =>  (index + 1) * lens.focalLength }.sum
    }.sum

    println(s"Result 2: $result")
  }

  private def hash(code: String) = {
    code.toCharArray.toVector.foldLeft(0)((hash, c) => ((hash + c) * 17) % 256)
  }
}

case class Lens(label: String, focalLength: Int)

object Day15 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2023/day15.txt")
    Day15(value).solve1()
    Day15(value).solve2()
  }

}
