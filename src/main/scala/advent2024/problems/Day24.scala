package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem}

import java.lang.Long.parseLong
import scala.annotation.tailrec

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (initialState, graph) = parse()
    val (zWires, finalState) = runSimulation(initialState, graph)

    val result = wiresToLong(zWires, finalState)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (initialState, graph) = parse()
    val xWires = initialState.keys.filter(_.startsWith("x")).toVector.sorted.reverse
    val yWires = initialState.keys.filter(_.startsWith("y")).toVector.sorted.reverse

    @tailrec
    def findMistakes(graph: Map[String, (Vector[String], String)], bit: Int, swaps: Vector[(String, String)], carry: Option[String]): (Vector[(String, String)], Map[String, (Vector[String], String)]) = {
      if (swaps.size == 4) (swaps, graph)
      else if(bit == 45) {
        println(s"Reached the end, but only ${swaps.size} swaps found")
        (swaps, graph)
      }
      else {
        val xWire = f"x$bit%02d"
        val yWire = f"y$bit%02d"
        val zWire = f"z$bit%02d"

        val halfSum = graph.find { case (_, (sources, operation)) =>
          sources == Vector(xWire, yWire) && operation == "XOR"
        }.get._1

        val halfCarry = graph.find { case (_, (sources, operation)) =>
          sources == Vector(xWire, yWire) && operation == "AND"
        }.get._1

        val fullSum = if (bit == 0) halfSum else {
          val maybeFullSum = graph.find { case (_, (sources, operation)) =>
            sources == Vector(halfSum, carry.get).sorted && operation == "XOR"
          }.map(_._1)

          if (maybeFullSum.isDefined) maybeFullSum.get else {
            // Try swapping the half sum and the half carry
            graph.find { case (_, (sources, operation)) =>
              sources == Vector(halfCarry, carry.get).sorted && operation == "XOR"
            }.get._1
          }
        }

        val (graphAfterSumCorrection, updatedSwaps) = if (fullSum != zWire) {
          // Full sum is not the z wire it should be – swap it with the correct z wire
          val newGraph = graph +
            (zWire -> (Vector(halfSum, carry.get).sorted, "XOR")) +
            (fullSum -> graph(zWire))

          val newSwaps = swaps :+ (zWire, fullSum)

          (newGraph, newSwaps)
        } else if (graph(fullSum)._1.contains(halfCarry)) {
          // Swap the half sum and half carry wires
          val newGraph = graph +
            (halfSum -> graph(halfCarry)) +
            (halfCarry -> graph(halfSum))

          val newSwaps = swaps :+ (halfSum, halfCarry)

          (newGraph, newSwaps)
        } else {
          (graph, swaps)
        }

        // These might have changed, recalculate them with the updated graph
        val newHalfSum = if (graphAfterSumCorrection == graph) halfSum else graphAfterSumCorrection.find { case (_, (sources, operation)) =>
          sources == Vector(xWire, yWire) && operation == "XOR"
        }.get._1

        val newHalfCarry = if (graphAfterSumCorrection == graph) halfCarry else graphAfterSumCorrection.find { case (_, (sources, operation)) =>
          sources == Vector(xWire, yWire) && operation == "AND"
        }.get._1

        val fullCarry = if (bit == 0) halfCarry else {
          val maybeTuple = graphAfterSumCorrection.find { case (_, (sources, operation)) =>
            sources == Vector(newHalfSum, carry.get).sorted && operation == "AND"
          }
          maybeTuple.get._1
        }

        val nextCarry = if (bit == 0) halfCarry else {
          graphAfterSumCorrection.find { case (_, (sources, operation)) =>
            sources == Vector(newHalfCarry, fullCarry).sorted && operation == "OR"
          }.get._1
        }

        findMistakes(graphAfterSumCorrection, bit + 1, updatedSwaps, Some(nextCarry))
      }
    }

    val (swaps, fixedGraph) = findMistakes(graph, 0, Vector(), None)

    val result = swaps.flatMap { case (a, b) => Vector(a, b) }.sorted.mkString(",")

    val (fixedZWires, fixedFinalState) = runSimulation(initialState, fixedGraph)
    val fixedZ = wiresToLong(fixedZWires, fixedFinalState)

    val x = wiresToLong(xWires, initialState)
    val y = wiresToLong(yWires, initialState)
    val expectedZ = x + y

    if (fixedZ != x + y) println(s"System not fixed: expected $expectedZ but was $fixedZ")

    println(s"Result 2: $result")
  }

  private def runSimulation(initialState: Map[String, Int], graph: Map[String, (Vector[String], String)]) = {
    val zWires = graph.keySet.filter(_.startsWith("z")).toVector.sorted.reverse

    @tailrec
    def helper(state: Map[String, Int]): Map[String, Int] = {
      if (zWires.forall(state.contains)) state
      else {
        val canProcess = graph.filter {
          case (dest, (sources, _)) => !state.contains(dest) && sources.forall(state.contains)
        }

        val newState = canProcess.map {
          case (dest, (Vector(x, y), operation)) => dest -> (operation match {
            case "AND" => state(x) & state(y)
            case "OR" => state(x) | state(y)
            case "XOR" => state(x) ^ state(y)
          })
        }

        helper(state ++ newState)
      }
    }

    val finalState = helper(initialState)
    (zWires, finalState)
  }

  private def wiresToLong(wires: Vector[String], state: Map[String, Int]) = {
    val bitString = wires.map(state).mkString("")
    parseLong(bitString, 2)
  }

  private def parse(): (Map[String, Int], Map[String, (Vector[String], String)]) = {
    val (initialStateLines, graphLines) = input.span(_.nonEmpty)

    val initialStatePattern = "([xy\\d]+): ([01])".r
    val initialState = initialStateLines.map {
      case initialStatePattern(label, value) => label -> value.toInt
    }.toMap

    val graphLinePattern = "([a-z\\d]+) (AND|OR|XOR) ([a-z\\d]+) -> ([a-z\\d]+)".r
    val graph = graphLines.tail.map {
      case graphLinePattern(lhs, operationString, rhs, dest) =>
        dest -> (Vector(lhs, rhs).sorted, operationString)
    }.toMap

    (initialState, graph)
  }
}

case object Day24 extends App {
  val input = Files.lines("2024/day24.txt")
  val problem = Day24(input)
  problem.solve1()
  problem.solve2()
}