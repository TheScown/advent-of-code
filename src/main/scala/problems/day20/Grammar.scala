package space.scown.advent2023
package problems.day20

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case object Grammar extends RegexParsers {

  def modules = module*

  def module: Parser[Module] = (broadcaster | flipFlop | conjunction)

  def broadcaster: Parser[Broadcaster] = ("broadcaster" <~ arrow) ~> identifiers ^^ (identifiers => Broadcaster(identifiers))

  def conjunction: Parser[Conjunction] = (("&" ~> identifier) <~ arrow) ~ identifiers ^^ {
    case name ~ destinations => Conjunction(name, destinations, Map())
  }

  def flipFlop: Parser[FlipFlop] = (("%" ~> identifier) <~ arrow) ~ identifiers ^^ {
    case name ~ destinations => FlipFlop(name, destinations, on = false)
  }

  def identifiers: Parser[List[String]] = repsep(identifier, ", ")
  def identifier: Parser[String] = "[a-z]+".r

  def arrow: Parser[String] = "->".r

}
