package space.scown.adventofcode2019
package intcode

import scala.annotation.tailrec

case class IntcodeComputer(program: Vector[Int], input: () => Int = () => 42, output: Int => Unit = _ => ()) {

  def execute(): Vector[Int] = {
    @tailrec
    def helper(pc: Int, memory: Vector[Int]): Vector[Int] = {
      println(s"$pc, $memory")

      Instruction.fromInt(memory(pc)) match {
        case Instruction(1, positionModes) => helper(pc + 4, add(pc, memory, positionModes))
        case Instruction(2, positionModes) => helper(pc + 4, multiply(pc, memory, positionModes))
        case Instruction(3, positionModes) => helper(pc + 2, read(pc, memory, positionModes))
        case Instruction(4, positionModes) => helper(pc + 2, write(pc, memory, positionModes))
        case Instruction(99, _) => memory
        case x => throw new IllegalStateException(s"Illegal opcode $x at $pc. State: $memory")
      }
    }

    helper(0, program)
  }

  private def add(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    memory.updated(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) + positionModes(1)(pc, memory))
  }

  private def multiply(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    memory.updated(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) * positionModes(1)(pc, memory))
  }

  private def read(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    memory.updated(positionModes(0).dest(pc, memory), input())
  }

  private def write(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    output(positionModes(0)(pc, memory))
    memory
  }
}

case class Instruction(opcode: Int, positionModes: Vector[PositionMode])

object Instruction {

  def fromInt(rawOpcode: Int): Instruction = {
    val opcode = rawOpcode % 100
    val positionModeInt = (rawOpcode - opcode) / 100
    val positionModes = (1 to 3).map { m =>
      PositionMode.fromInt((positionModeInt / Math.pow(10, m - 1).toInt) % Math.pow(10, m).toInt, m)
    }.toVector

    Instruction(opcode, positionModes)
  }

}

sealed trait PositionMode {
  def apply(pc: Int, memory: Vector[Int]): Int

  def dest(pc: Int, memory: Vector[Int]): Int
}

case class Position(offset: Int) extends PositionMode {
  override def apply(pc: Int, memory: Vector[Int]): Int = {
    val pointer = memory(pc + offset)
    memory(pointer)
  }

  override def dest(pc: Int, memory: Vector[Int]): Int = {
    memory(pc + offset)
  }
}

case class Immediate(offset: Int) extends PositionMode {
  override def apply(pc: Int, memory: Vector[Int]): Int = {
    memory(pc + offset)
  }

  override def dest(pc: Int, memory: Vector[Int]): Int = {
    pc + offset
  }
}

object PositionMode {

  def fromInt(p: Int, offset: Int): PositionMode = p match {
    case 0 => Position(offset)
    case 1 => Immediate(offset)
    case _ => throw new IllegalStateException(s"Invalid position mode $p")
  }

}
