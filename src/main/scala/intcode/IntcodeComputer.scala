package space.scown.adventofcode2019
package intcode

import scala.annotation.tailrec

case class IntcodeComputer(program: Vector[Int], input: () => Int = () => 42, output: Int => Unit = _ => ()) {

  def execute(): Vector[Int] = {
    @tailrec
    def helper(pc: Int, memory: Vector[Int]): Vector[Int] = {
      println(memory)

      memory(pc) match {
        case 1 => helper(pc + 4, add(pc, memory))
        case 2 => helper(pc + 4, multiply(pc, memory))
        case 3 => helper(pc + 1, read(pc, memory))
        case 4 => helper(pc + 1, write(pc, memory))
        case 99 => memory
        case x => throw new IllegalStateException(s"Illegal opcode $x at $pc. State: $memory")
      }
    }

    helper(0, program)
  }

  private def add(pc: Int, memory: Vector[Int]) = {
    memory.updated(memory(pc + 3), memory(memory(pc + 1)) + memory(memory(pc + 2)))
  }

  private def multiply(pc: Int, memory: Vector[Int]) = {
    memory.updated(memory(pc + 3), memory(memory(pc + 1)) * memory(memory(pc + 2)))
  }

  private def read(pc: Int, memory: Vector[Int]) = {
    memory.updated(memory(pc + 1), input())
  }

  private def write(pc: Int, memory: Vector[Int]) = {
    output(memory(pc + 1))
    memory
  }
}
