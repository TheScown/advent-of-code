package space.scown.advent2023
package problems.day20

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.parsing.input.PagedSeqReader

case class Day20(modules: Seq[Module]) extends Problem {

  override def solve1(): Unit = {
    val fixedConjunctions = modules.map {
      case Conjunction(name, destinations, memory) =>
        val inputs = modules.filter(_.destinations.contains(name)).map(_.name -> false)
        Conjunction(name, destinations, memory ++ inputs)
      case m => m
    }

    val index = fixedConjunctions.groupBy(_.name).view.mapValues(_.head).toMap

    @tailrec
    def helper(queue: Queue[(String, String, Boolean)], index: Map[String, Module], lowPulseCount: Int, highPulseCount: Int): (Int, Int, Map[String, Module]) = {
      if (queue.isEmpty) (lowPulseCount, highPulseCount, index)
      else {
        val (next, nextQueue) = queue.dequeue
        val (sourceModule, destinationModule, nextPulse) = next

//        println(s"$sourceModule -> $nextPulse -> $destinationModule")

        val nextModule = index.getOrElse(destinationModule, Dummy(destinationModule))
        val (updatedModule, newPulses) = nextModule(index, (sourceModule, nextPulse))
        val newQueue = newPulses.foldLeft(nextQueue)((queue, pulse) => queue.enqueue(pulse))

        val newLowPulseCount = if (nextPulse) lowPulseCount else lowPulseCount + 1
        val newHighPulseCount = if (nextPulse) highPulseCount + 1 else highPulseCount

        helper(newQueue, index + (destinationModule -> updatedModule), newLowPulseCount, newHighPulseCount)
      }
    }

    val (lowPulses, highPulses, _) = (1 to 1000).foldLeft((0, 0, index))((acc, _) => {
//      println(s"$acc")

      helper(Queue(("button", "broadcaster", false)), acc._3, acc._1, acc._2)
    })
    val result = lowPulses * highPulses

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val fixedConjunctions = modules.map {
      case Conjunction(name, destinations, memory) =>
        val inputs = modules.filter(_.destinations.contains(name)).map(_.name -> false)
        Conjunction(name, destinations, memory ++ inputs)
      case m => m
    }

    val index = fixedConjunctions.groupBy(_.name).view.mapValues(_.head).toMap

    @tailrec
    def helper(queue: Queue[(String, String, Boolean)], index: Map[String, Module], targetSource: String): (Map[String, Module], Boolean) = {
      if (queue.isEmpty) (index, false)
      else {
        val (next, nextQueue) = queue.dequeue
        val (sourceModule, destinationModule, nextPulse) = next

//        println(s"$sourceModule -> $nextPulse -> $destinationModule")

        if (sourceModule == targetSource && destinationModule == "zh" && nextPulse) (index, true)
        else {
          val nextModule = index.getOrElse(destinationModule, Dummy(destinationModule))
          val (updatedModule, newPulses) = nextModule(index, (sourceModule, nextPulse))
          val newQueue = newPulses.foldLeft(nextQueue)((queue, pulse) => queue.enqueue(pulse))

          helper(newQueue, index + (destinationModule -> updatedModule), targetSource)
        }
      }
    }

    @tailrec
    def findCycle(targetSource: String, index: Map[String, Module], buttonCount: Int): Int = {
      val (updatedIndex, done) = helper(Queue(("button", "broadcaster", false)), index, targetSource)

      if (done) buttonCount + 1
      else findCycle(targetSource, updatedIndex, buttonCount + 1)
    }

    val cycleLengths = List("vd", "ns", "bh", "dl").map(targetSource => findCycle(targetSource, index, 0).toLong)

    println(s"Cycle lengths: $cycleLengths")

    val result = cycleLengths.reduce(lcm)

    println(s"Result 2: $result")
  }

  private def lcm(x: Long, y: Long) = {
    x * (y / gcd(x, y))
  }

  private def gcd(x: Long, y: Long): Long = {
    @tailrec
    def helper(a: Long, b: Long): Long = {
      if (b == 0) a
      else helper(b, a % b)
    }

    helper(Math.max(x, y), Math.min(x, y))
  }
}

object Day20 {
  def main(args: Array[String]): Unit = {
    val reader = new PagedSeqReader(Files.pagedSequence("day20.txt"))
    Grammar.parseAll[Seq[Module]](Grammar.modules, reader) match {
      case Grammar.Success(modules, _) =>
        val day = Day20(modules)
        time(() => day.solve1())
        time(() => day.solve2())
      case error@Grammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
      case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
    }
  }
}
