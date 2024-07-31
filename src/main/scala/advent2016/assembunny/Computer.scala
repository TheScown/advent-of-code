package space.scown.adventofcode
package advent2016.assembunny

import scala.annotation.tailrec

case object Computer {

  def run(instructions: Vector[Instruction], registers: Map[Char, Int]): Int = {
    @tailrec
    def helper(pc: Int, registers: Map[Char, Int]): Int = {
      if (pc >= instructions.size) registers('a')
      else {
        instructions(pc) match {
          case Inc(Register(x)) => helper(pc + 1, registers + (x -> (registers(x) + 1)))
          case Dec(Register(x)) => helper(pc + 1, registers + (x -> (registers(x) - 1)))
          case Copy(Register(x), Register(y)) =>  helper(pc + 1, registers + (y -> registers(x)))
          case Copy(Value(x), Register(y)) =>  helper(pc + 1, registers + (y -> x))
          case Jnz(Register(x), Value(y)) =>
            if (registers(x) == 0) helper(pc + 1, registers)
            else helper(pc + y, registers)
          case Jnz(Value(x), Value(y)) =>
            if (x == 0) helper(pc + 1, registers)
            else helper(pc + y, registers)
        }
      }
    }

    helper(0, registers)
  }

}
