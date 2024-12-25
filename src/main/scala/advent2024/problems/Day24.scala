package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem}

import java.lang.Long.parseLong
import scala.annotation.tailrec

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (initialState, graph) = parse()
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

    val result = wiresToLong(zWires, finalState)

    println(s"Result 1: $result")
    println(result.toBinaryString)
  }

  override def solve2(): Unit = {
    val (initialState, graph) = parse()
    val xWires = initialState.keys.filter(_.startsWith("x")).toVector.sorted.reverse
    val yWires = initialState.keys.filter(_.startsWith("y")).toVector.sorted.reverse
    val zWires = graph.keySet.filter(_.startsWith("z")).toVector.sorted.reverse

    def dependencies(wire: String): Set[String] = {
      if (wire.startsWith("x") || wire.startsWith("y")) Set()
      else {
        val directDependencies = graph(wire)._1.toSet
        directDependencies union directDependencies.map(dependencies).reduce(_ union _)
      }
    }

    val x = wiresToLong(xWires, initialState)
    val y = wiresToLong(yWires, initialState)
    println(s"_${x.toBinaryString}")
    println(s"_${y.toBinaryString}")
    println(s"${(x+y).toBinaryString}")
//    println(s"${(x)}")
//    println(s"${(y)}")
//    println(s"${(x+y)}")

    zWires.foreach { zWire =>
      val dependsOn = dependencies(zWire).toVector.sorted
      println(zWire, dependsOn)
    }
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
        dest -> (Vector(lhs, rhs), operationString)
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