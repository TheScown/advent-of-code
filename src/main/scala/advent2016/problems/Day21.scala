package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val input = new StringBuilder("abcdefgh")

    val result = instructions.foldLeft(input) { (s, instruction) =>
      instruction match {
        case SwapPosition(x, y) =>
          val temp = s(x)
          s(x) = s(y)
          s(y) = temp
          s
        case SwapLetter(x, y) =>
          s.indices.foreach { i =>
            if (s(i) == x) s(i) = y
            else if (s(i) == y) s(i) = x
          }
          s
        case RotateLeft(x) =>
          val (start, end) = s.splitAt(x)
          end.append(start)
        case RotateRight(x) =>
          val (start, end) = s.splitAt(s.length - x)
          end.append(start)
        case RotateLetter(x) =>
          val index = s.indexOf(x)
          val rotation = (if (index >= 4) index + 2 else index + 1) % s.length()
          val (start, end) = s.splitAt(s.length - rotation)
          end.append(start)
        case Reverse(x, y) =>
          val toReverse = s.substring(x, y + 1)
          val reversed = toReverse.reverse
          for {
            i <- x to y
          } s(i) = reversed(i - x)
          s
        case Move(x, y) =>
          val toMove = s(x)
          s.deleteCharAt(x)
          s.insert(y, toMove)
      }
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse().reverse

    val input = new StringBuilder("fbgdceah")

    val result = instructions.foldLeft(input) { (s, instruction) =>
      instruction match {
        case SwapPosition(x, y) =>
          val temp = s(x)
          s(x) = s(y)
          s(y) = temp
          s
        case SwapLetter(x, y) =>
          s.indices.foreach { i =>
            if (s(i) == x) s(i) = y
            else if (s(i) == y) s(i) = x
          }
          s
        case RotateLeft(x) =>
          val (start, end) = s.splitAt(s.length - x)
          end.append(start)
        case RotateRight(x) =>
          val (start, end) = s.splitAt(x)
          end.append(start)
        case RotateLetter(x) =>
          // This turns out not to be a bijection for length != 8
          val currentIndex = s.indexOf(x)
          val originalIndex = if (currentIndex % 2 == 1) {
            currentIndex / 2
          } else {
            val adjusted = if (currentIndex == 0) s.length() else currentIndex
            adjusted / 2 + 3
          }
          // This is a right rotate to the correct position
          val rotation = (originalIndex - currentIndex + s.length()) % s.length()
          val (start, end) = s.splitAt(s.length() - rotation)
          end.append(start)
        case Reverse(x, y) =>
          val toReverse = s.substring(x, y + 1)
          val reversed = toReverse.reverse
          for {
            i <- x to y
          } s(i) = reversed(i - x)
          s
        case Move(x, y) =>
          val toMove = s(y)
          s.deleteCharAt(y)
          s.insert(x, toMove)
      }
    }

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Instruction] = {
    val swapPosition = "swap position (\\d+) with position (\\d+)".r
    val swapLetter = "swap letter (.) with letter (.)".r
    val rotateLeft = "rotate left (\\d+) steps?".r
    val rotateRight = "rotate right (\\d+) steps?".r
    val rotateLetter = "rotate based on position of letter (.)".r
    val reverse = "reverse positions (\\d+) through (\\d+)".r
    val move = "move position (\\d+) to position (\\d+)".r

    input.map {
      case swapPosition(x, y) => SwapPosition(x.toInt, y.toInt)
      case swapLetter(x, y) => SwapLetter(x.head, y.head)
      case rotateLeft(x) => RotateLeft(x.toInt)
      case rotateRight(x) => RotateRight(x.toInt)
      case rotateLetter(x) => RotateLetter(x.head)
      case reverse(x, y) => Reverse(x.toInt, y.toInt)
      case move(x, y) => Move(x.toInt, y.toInt)
    }
  }

  sealed trait Instruction
  private case class SwapPosition(x: Int, y: Int) extends Instruction
  private case class SwapLetter(x: Char, y: Char) extends Instruction
  private case class RotateLeft(x: Int) extends Instruction
  private case class RotateRight(x: Int) extends Instruction
  private case class RotateLetter(x: Char) extends Instruction
  private case class Reverse(x: Int, y: Int) extends Instruction
  private case class Move(x: Int, y: Int) extends Instruction
}

case object Day21 extends App {
  val input = Files.lines("2016/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  problem.solve2()
}
