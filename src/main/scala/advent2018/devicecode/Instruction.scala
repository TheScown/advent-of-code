package space.scown.adventofcode
package advent2018.devicecode

case class Instruction[T](opcode: T, a: Int, b: Int, c: Int)

sealed trait Operation
case object Addr extends Operation
case object Addi extends Operation
case object Mulr extends Operation
case object Muli extends Operation
case object Banr extends Operation
case object Bani extends Operation
case object Borr extends Operation
case object Bori extends Operation
case object Setr extends Operation
case object Seti extends Operation
case object Gtir extends Operation
case object Gtri extends Operation
case object Gtrr extends Operation
case object Eqir extends Operation
case object Eqri extends Operation
case object Eqrr extends Operation

case object Instruction {
  type Func = (Vector[Int], Int, Int, Int) => Vector[Int]

  def addr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) + registers(b))
  }

  def addi(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) + b)
  }

  def mulr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) * registers(b))
  }

  def muli(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) * b)
  }

  def banr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) & registers(b))
  }

  def bani(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) & b)
  }

  def borr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) | registers(b))
  }

  def bori(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) | b)
  }

  def setr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a))
  }

  def seti(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, a)
  }

  def gtir(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (a > registers(b)) 1 else 0)
  }

  def gtri(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) > b) 1 else 0)
  }

  def gtrr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) > registers(b)) 1 else 0)
  }

  def eqir(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (a == registers(b)) 1 else 0)
  }

  def eqri(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) == b) 1 else 0)
  }

  def eqrr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) == registers(b)) 1 else 0)
  }

  def parse(input: Vector[String]): (Vector[Instruction[Func]], Int) = {
    val ipDirective = input.head
    val instructionStrings = input.tail

    val ipRegister = ipDirective.split(" ").last.toInt

    val instructionPattern = "([a-z]{4}) (\\d+) (\\d+) (\\d+)".r

    val instructions = instructionStrings.map {
      case instructionPattern(op, a, b, c) => op match {
        case "addr" => Instruction[Func](addr, a.toInt, b.toInt, c.toInt)
        case "addi" => Instruction[Func](addi, a.toInt, b.toInt, c.toInt)
        case "mulr" => Instruction[Func](mulr, a.toInt, b.toInt, c.toInt)
        case "muli" => Instruction[Func](muli, a.toInt, b.toInt, c.toInt)
        case "banr" => Instruction[Func](banr, a.toInt, b.toInt, c.toInt)
        case "bani" => Instruction[Func](bani, a.toInt, b.toInt, c.toInt)
        case "borr" => Instruction[Func](borr, a.toInt, b.toInt, c.toInt)
        case "bori" => Instruction[Func](bori, a.toInt, b.toInt, c.toInt)
        case "setr" => Instruction[Func](setr, a.toInt, b.toInt, c.toInt)
        case "seti" => Instruction[Func](seti, a.toInt, b.toInt, c.toInt)
        case "gtir" => Instruction[Func](gtir, a.toInt, b.toInt, c.toInt)
        case "gtri" => Instruction[Func](gtri, a.toInt, b.toInt, c.toInt)
        case "gtrr" => Instruction[Func](gtrr, a.toInt, b.toInt, c.toInt)
        case "eqir" => Instruction[Func](eqir, a.toInt, b.toInt, c.toInt)
        case "eqri" => Instruction[Func](eqri, a.toInt, b.toInt, c.toInt)
        case "eqrr" => Instruction[Func](eqrr, a.toInt, b.toInt, c.toInt)
      }
    }

    (instructions, ipRegister)
  }
}
