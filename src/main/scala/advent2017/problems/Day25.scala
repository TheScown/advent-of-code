package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{PagedSeq, PagedSeqReader}

case class Day25(input: PagedSeq[Char]) extends Problem {
  override def solve1(): Unit = {
    val machine = Grammar.parse(Grammar.machine, new PagedSeqReader(input)) match {
      case Grammar.Success(machine, _) => machine
      case Grammar.NoSuccess.I(err, _) => throw new IllegalArgumentException(err)
    }

    @tailrec
    def helper(position: Int, currentState: Char, tape: Map[Int, Int], remainingInstructions: Int): Int = {
      if (remainingInstructions == 0) {
        tape.map{ case (_, v) => v }.sum
      }
      else {
        val state = machine.states(currentState)
        val currentValue = tape.getOrElse(position, 0)
        val actions = state.actions(currentValue)

        val (newPosition, updatedTape, newState) = actions.foldLeft((position, tape, currentState)) { (acc, action) => acc match {
          case (position, tape, state) => action match {
            case Write(x) => (position, tape + (position -> x), state)
            case Move(x) => (position + x, tape, state)
            case Continue(s) => (position, tape, s)
          }
        } }

        helper(newPosition, newState, updatedTape, remainingInstructions - 1)
      }
    }

    val result =  helper(0, machine.startState, Map(), machine.stepsToTake)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Result 2: Reboot the printer!")
  }

  case object Grammar extends RegexParsers {

    def machine: Parser[Machine] = begin ~ diagnostic ~ states ^^ {
      case startState ~ diagnosticSteps ~ states => Machine(startState, diagnosticSteps, states.map(s => s.label -> s).toMap)
    }

    def begin: Parser[Char] = "Begin in state " ~> label <~ ".".r

    def diagnostic: Parser[Int] = "Perform a diagnostic checksum after" ~> number <~ "steps.".r

    def states: Parser[List[State]] = rep(state)

    def state: Parser[State] = stateHeader ~ rep(condition ~ rep(action)) ^^ {
      case state ~ xs => State(
        state,
        xs.map {
          case condition ~ actions => condition -> actions
        }.toMap
      )
    }

    def stateHeader: Parser[Char] = "In state" ~> label <~ ":".r

    def condition: Parser[Int] = "If the current value is" ~> number <~ ":".r

    def action: Parser[Action] = "-" ~> (write | move | continue)
    def write: Parser[Write] = "Write the value" ~> number <~ "\\.".r ^^ (v => Write(v))
    def move: Parser[Move] = "Move one slot to the" ~> direction <~ "\\.".r ^^ {
      case "left" => Move(-1)
      case "right" => Move(1)
    }
    def continue: Parser[Continue] = "Continue with state" ~> label <~ "\\.".r ^^ (next => Continue(next))

    def label: Parser[Char] = "([A-Z])".r ^^ (_.head)
    def direction: Parser[String] = "left|right".r
    def number: Parser[Int] = "(\\d+)".r ^^ (s => s.toInt)
  }

  case class Machine(startState: Char, stepsToTake: Int, states: Map[Char, State])
  case class State(label: Char, actions: Map[Int, Seq[Action]])

  sealed trait Action
  case class Write(value: Int) extends Action
  case class Move(direction: Int) extends Action
  case class Continue(nextState: Char) extends Action
}

case object Day25 extends App {
  val input = Files.pagedSequence("2017/day25.txt")
  val problem = Day25(input)
  Timer.time(() => problem.solve1())
  problem.solve2()
}
