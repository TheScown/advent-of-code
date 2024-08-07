package space.scown.adventofcode
package advent2016.assembunny

import scala.annotation.tailrec

case object Computer {

  def run(instructions: Vector[Instruction], registers: Map[Char, Int]): LazyList[Int] = {
    def helper(pc: Int, registers: Map[Char, Int], instructions: Vector[Instruction]): LazyList[Int] = {
      if (pc >= instructions.size) LazyList(registers('a'))
      else {
        instructions(pc) match {
          case Inc(Register(x)) => helper(pc + 1, registers + (x -> (registers(x) + 1)), instructions)
          case Dec(Register(x)) => helper(pc + 1, registers + (x -> (registers(x) - 1)), instructions)
          case Copy(Register(x), Register(y)) =>  helper(pc + 1, registers + (y -> registers(x)), instructions)
          case Copy(Value(x), Register(y)) =>  helper(pc + 1, registers + (y -> x), instructions)
          case Jnz(Register(x), Value(y)) =>
            if (registers(x) == 0) helper(pc + 1, registers, instructions)
            else helper(pc + y, registers, instructions)
          case Jnz(Value(x), Value(y)) =>
            if (x == 0) helper(pc + 1, registers, instructions)
            else helper(pc + y, registers, instructions)
          case Jnz(Register(x), Register(y)) =>
            if (registers(x) == 0) helper(pc + 1, registers, instructions)
            else helper(pc + registers(y), registers, instructions)
          case Jnz(Value(x), Register(y)) =>
            if (x == 0) helper(pc + 1, registers, instructions)
            else helper(pc + registers(y), registers, instructions)
          case Out(Register(x)) =>
            registers(x) #:: helper(pc + 1, registers, instructions)
          case Out(Value(x)) =>
            x #:: helper(pc + 1, registers, instructions)
          case Tgl(Value(x)) =>
            if (pc + x >= instructions.size) helper(pc + 1, registers, instructions)
            else {
              val newInstruction = instructions(pc + x) match {
                case Inc(s) => Dec(s)
                case Dec(s) => Inc(s)
                case Tgl(s) => Inc(s)
                case Copy(s1, s2) => Jnz(s1, s2)
                case Jnz(s1, s2) => Copy(s1, s2)
              }
              val toOptimise = instructions.updated(pc + x, newInstruction)
              val optimised = OptimizingGrammar.optimise(toOptimise)
              helper(pc + 1, registers, optimised)
            }
          case Tgl(Register(x)) =>
            if (pc + registers(x) >= instructions.size) helper(pc + 1, registers, instructions)
            else {
              val newInstruction = instructions(pc + registers(x)) match {
                case Inc(s) => Dec(s)
                case Dec(s) => Inc(s)
                case Tgl(s) => Inc(s)
                case Copy(s1, s2) => Jnz(s1, s2)
                case Jnz(s1, s2) => Copy(s1, s2)
              }
              val toOptimise = instructions.updated(pc + registers(x), newInstruction)
              val optimised = OptimizingGrammar.optimise(toOptimise)
              helper(pc + 1, registers, optimised)
            }
          case Add(Register(src), Register(dest)) =>
            helper(pc + 1, registers + (dest -> (registers(dest) + registers(src))) + (src -> 0), instructions)
          case Multiply(Register(x), Register(y), Register(dest)) =>
            helper(pc + 1, registers + (dest -> (registers(dest) + registers(x) * registers(y))) + (x -> 0) + (y -> 0), instructions)
          case _ => helper(pc + 1, registers, instructions)
        }
      }
    }

    helper(0, registers, OptimizingGrammar.optimise(instructions))
  }

}
