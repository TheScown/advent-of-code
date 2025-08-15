package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{AsciiAdapter, IntcodeComputer, IntcodeProgram}
import lib.{BFS, Files, Problem}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class Day25(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program)
    val asciiComputer = AsciiAdapter(computer.execute())

    // Determined by inspection
    val bannedItems = Set(
      "photons",
      "escape pod",
      "molten lava",
      "infinite loop",
      "giant electromagnet"
    )

    val start = State(asciiComputer, "Start", Set())

    val inPosition = BFS.solve(start)(state => state.location == "Security Checkpoint" && state.items.size == 8) { (state, _) =>
      val output = state.computer.stringOutput
      val lines = output.split("\n").toVector
      val location = parseOutput(lines)

      if (location.name == "Pressure-Sensitive Floor") Seq()
      else {
        val validItems = location.items.filter(!bannedItems.contains(_))
        val afterCollection = validItems.foldLeft(state.computer) { (computer, item) =>
          val command = s"take $item\n"
          computer.sendString(command)
        }

        location.doors.toVector.map { door =>
          val command = s"$door\n"
          State(afterCollection.sendString(command), location.name, state.items ++ validItems)
        }
      }
    }.get.value

    val items = inPosition.items.toVector
    val thingsToDrop = (for {
      i <- 0 to items.size
    } yield items.combinations(i)).flatten

    val locations = thingsToDrop.map { items =>
      val withDroppedItems = items.foldLeft(inPosition.computer) { (computer, item) =>
        computer.sendString(s"drop $item\n")
      }

      // Determined by inspection
      val finalComputer = withDroppedItems.sendString("south\n")

      (parseOutput(finalComputer.stringOutput.split("\n").toVector), finalComputer.stringOutput)
    }

    val correctChoice = locations.find(_._1.name == "Pressure-Sensitive Floor").get
    val resultPattern = "(?s).*typing (\\d+).*".r

    val result = correctChoice._2 match {
      case resultPattern(password) => password
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Align the Warp Drive!")
  }

  @tailrec
  private def parseOutput(lines: Vector[String]): Location = {
    val namePattern = "== (.*?) ==".r

    val nameAndRest = lines.dropWhile(!namePattern.matches(_))

    val name = nameAndRest.head match {
      case namePattern(name) => name
    }

    val secondLocation = nameAndRest.tail.dropWhile(!_.startsWith("=="))

    if (secondLocation.nonEmpty) parseOutput(secondLocation)
    else {
      val doorsHeadingAndRest = nameAndRest.dropWhile(_ != "Doors here lead:")
      val directions = doorsHeadingAndRest.tail
        .takeWhile(_.nonEmpty)
        .map(_.replace("- ", ""))
        .toSet

      val itemsHeadingAndRest = doorsHeadingAndRest.dropWhile(_ != "Items here:")

      val items = if (itemsHeadingAndRest.isEmpty) Set[String]()
      else {
        itemsHeadingAndRest.tail
          .takeWhile(_.nonEmpty)
          .map(_.replace("- ", ""))
          .toSet
      }

      Location(name, directions, items)
    }

  }

  @tailrec
  private def interactivePrompt(asciiComputer: AsciiAdapter): String = {
    print(asciiComputer.stringOutput)

    val nextLine = readLine()

    val newComputer = asciiComputer.sendString(s"$nextLine\n")

    if (newComputer.output.terminal) {
      print(newComputer.stringOutput)
      "Result"
    }
    else interactivePrompt(newComputer)
  }

  case class Location(name: String, doors: Set[String], items: Set[String])

  case class State(computer: AsciiAdapter, location: String, items: Set[String]) {
    override def hashCode(): Int = location.hashCode() + 37 * items.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case State(_, l, is) => l == location && is == items
      case _ => false
    }
  }
}

case object Day25 extends App {
  val input = Files.lines("2019/day25.txt")
  val problem = Day25(input)
  problem.solve1()
  problem.solve2()
}
