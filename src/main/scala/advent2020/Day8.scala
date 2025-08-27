package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day8(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    @tailrec
    def helper(pc: Int, acc: Int, seenPcs: Set[Int]): Int = {
      if (seenPcs.contains(pc)) acc
      else {
        val (newPc, newAcc) = instructions(pc) match {
          case Nop(_) => (pc + 1, acc)
          case Acc(x) => (pc + 1, acc + x)
          case Jmp(x) => (pc + x, acc)
        }

        helper(newPc, newAcc, seenPcs + pc)
      }
    }

    val result = helper(0, 0, Set())

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    @tailrec
    def helper(instructions: Vector[Instruction], pc: Int, acc: Int, seenPcs: Set[Int]): (Int, Boolean) = {
      if (seenPcs.contains(pc)) (acc, false)
      else if (!instructions.indices.contains(pc)) (acc, true)
      else {
        val (newPc, newAcc) = instructions(pc) match {
          case Nop(_) => (pc + 1, acc)
          case Acc(x) => (pc + 1, acc + x)
          case Jmp(x) => (pc + x, acc)
        }

        helper(instructions, newPc, newAcc, seenPcs + pc)
      }
    }

    @tailrec
    def modifyAndRun(remainingInstructions: Vector[(Instruction, Int)]): Int = {
      remainingInstructions.head match {
        case (Acc(_), _) => modifyAndRun(remainingInstructions.tail)
        case (instruction, index) =>
          val updatedInstructions = instructions.updated(index, instruction match {
            case Nop(x) => Jmp(x)
            case Jmp(x) => Nop(x)
          })

          val (acc, terminated) = helper(updatedInstructions, 0, 0, Set())

          if (terminated) acc
          else modifyAndRun(remainingInstructions.tail)
      }
    }

    val result = modifyAndRun(instructions.zipWithIndex)

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Instruction] = {
    val nop = "nop ([+-]\\d+)".r
    val acc = "acc ([+-]\\d+)".r
    val jmp = "jmp ([+-]\\d+)".r

    input.map {
      case nop(value) => Nop(value.toInt)
      case acc(value) => Acc(value.toInt)
      case jmp(value) => Jmp(value.toInt)
    }
  }

  trait Instruction
  case class Nop(value: Int) extends Instruction
  case class Acc(value: Int) extends Instruction
  case class Jmp(value: Int) extends Instruction
}

case object Day8 extends App {
  val input = Files.lines("2020/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}