package space.scown.adventofcode
package advent2015.problems.day7

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case object Grammar extends RegexParsers {

  def operations: Parser[Map[String, Operation]] = (operation*) ^^ (operations => Map.from(operations))

  def operation: Parser[(String, Operation)] = (body <~ "->") ~ name ^^ {
    case body ~ name => (name, body)
  }

  def body: Parser[Operation] = and |
    fixedAnd |
    or |
    lshift |
    rshift |
    not |
    passthrough |
    fixed

  def fixed: Parser[Fixed] = number ^^ (number => Fixed(number))

  def passthrough: Parser[Passthrough] = name ^^ (s => Passthrough(s))

  def and: Parser[And] = (name <~ "AND") ~ name ^^ {
    case left ~ right => And(left, right)
  }

  def fixedAnd: Parser[FixedAnd] = (number <~ "AND") ~ name ^^ {
    case left ~ right => FixedAnd(left, right)
  }

  def or: Parser[Or] = (name <~ "OR") ~ name ^^ {
    case left ~ right => Or(left, right)
  }

  def lshift: Parser[LShift] = (name <~ "LSHIFT") ~ number ^^ {
    case left ~ right => LShift(left, right)
  }

  def rshift: Parser[RShift] = (name <~ "RSHIFT") ~ number ^^ {
    case left ~ right => RShift(left, right)
  }

  def not: Parser[Not] = "NOT" ~> name ^^ (name => Not(name))

  def name: Parser[String] = "[a-z]+".r

  def number: Parser[Char] = "0|[1-9][0-9]*".r ^^ (s => s.toInt.toChar)

}
