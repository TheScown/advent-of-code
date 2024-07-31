package space.scown.adventofcode
package advent2016.assembunny

import scala.util.parsing.combinator.RegexParsers

case object Grammar extends RegexParsers {
  def instruction: Parser[Instruction] = cpy | inc | dec | jnz | tgl

  private def cpy: Parser[Copy] = ("cpy" ~> (source ~ register)) ^^ {
    case source ~ register => Copy(source, register)
  }

  private def inc: Parser[Inc] = ("inc" ~> register) ^^ (register => Inc(register))

  private def dec: Parser[Dec] = ("dec" ~> register) ^^ (register => Dec(register))

  private def jnz: Parser[Jnz] = ("jnz" ~> (source ~ source)) ^^ {
    case source ~ number => Jnz(source, number)
  }

  private def tgl: Parser[Tgl] = ("tgl" ~> register) ^^ (register => Tgl(register))

  private def source = register | number
  private def register = "[abcd]".r ^^ (s => Register(s.head))
  private def number = "(-?\\d+)".r ^^ (s => Value(s.toInt))
}
