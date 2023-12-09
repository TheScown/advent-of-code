package space.scown.adventofcode2019
package intcode

case class IntcodeComputer(program: Vector[Int]) {

  def execute(input: LazyList[Int] = LazyList()): LazyList[(Vector[Int], Option[Int])] = {
    def helper(pc: Int, memory: Vector[Int], input: LazyList[Int]): LazyList[(Vector[Int], Option[Int])] = {
//      println(s"$pc, $memory")

      Instruction.fromInt(memory(pc)) match {
        case Instruction(1, positionModes) => helper(pc + 4, add(pc, memory, positionModes), input)
        case Instruction(2, positionModes) => helper(pc + 4, multiply(pc, memory, positionModes), input)
        case Instruction(3, positionModes) =>
          val (updatedMemory, remainingInput) = readIn(pc, memory, positionModes, input)
          helper(pc + 2, updatedMemory, remainingInput)
        case Instruction(4, positionModes) =>
          (memory, Some(positionModes(0)(pc, memory))) #:: helper(pc + 2, memory, input)
        case Instruction(5, positionModes) => helper(jumpTrue(pc, memory, positionModes), memory, input)
        case Instruction(6, positionModes) => helper(jumpFalse(pc, memory, positionModes), memory, input)
        case Instruction(7, positionModes) => helper(pc + 4, lessThan(pc, memory, positionModes), input)
        case Instruction(8, positionModes) => helper(pc + 4, equal(pc, memory, positionModes), input)
        case Instruction(99, _) => LazyList((memory, None))
        case x => throw new IllegalStateException(s"Illegal opcode $x at $pc. State: $memory")
      }
    }

    helper(0, program, input)
  }

  private def add(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    memory.updated(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) + positionModes(1)(pc, memory))
  }

  private def multiply(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    memory.updated(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) * positionModes(1)(pc, memory))
  }

  private def readIn(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode], input: LazyList[Int]): (Vector[Int], LazyList[Int]) = {
    input match {
      case next #:: remainder => (memory.updated(positionModes(0).dest(pc, memory), next), remainder)
      case LazyList() => throw new IllegalStateException(s"No more input. $pc, $memory")
    }
  }

  private def jumpTrue(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    val test = positionModes(0)(pc, memory)

    if (test != 0) positionModes(1)(pc, memory)
    else pc + 3
  }

  private def jumpFalse(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    val test = positionModes(0)(pc, memory)

    if (test == 0) positionModes(1)(pc, memory)
    else pc + 3
  }

  private def lessThan(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    val condition = positionModes(0)(pc, memory) < positionModes(1)(pc, memory)
    memory.updated(positionModes(2).dest(pc, memory), if (condition) 1 else 0)
  }

  private def equal(pc: Int, memory: Vector[Int], positionModes: Vector[PositionMode]) = {
    val condition = positionModes(0)(pc, memory) == positionModes(1)(pc, memory)
    memory.updated(positionModes(2).dest(pc, memory), if (condition) 1 else 0)
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
    throw new IllegalStateException(s"Attempting to write in immediate mode. $pc + $offset, $memory")
  }
}

object PositionMode {

  def fromInt(p: Int, offset: Int): PositionMode = p match {
    case 0 => Position(offset)
    case 1 => Immediate(offset)
    case _ => throw new IllegalStateException(s"Invalid position mode $p")
  }

}
