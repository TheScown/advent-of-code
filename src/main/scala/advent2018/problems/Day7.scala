package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.mutable

case class Day7(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val (steps, dependencies) = parse()
    val queue = mutable.PriorityQueue[Char]()(Ordering.Char.reverse)

    @tailrec
    def helper(enqueued: Set[Char], complete: Set[Char], history: Vector[Char]): String = {
      if (queue.isEmpty) history.mkString("")
      else {
        val next = queue.dequeue()
        val newlyCompleted = complete + next

        val toProcess = (steps -- enqueued).filter(c => dependencies(c).forall(newlyCompleted.contains))
        toProcess.foreach(queue.enqueue(_))

        helper(enqueued ++ toProcess, newlyCompleted, history :+ next)
      }
    }

    val initialSteps = steps.filterNot(dependencies.contains)
    initialSteps.foreach(queue.enqueue(_))

    val result = helper(initialSteps, Set(), Vector())
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (steps, dependencies) = parse()
    val queue = mutable.PriorityQueue[Char]()(Ordering.Char.reverse)

    val baseTime = 60
    val workers = 5

    def duration(step: Char): Int = (step - 'A') + 1 + baseTime

    @tailrec
    def helper(enqueued: Set[Char], inProgress: Set[(Char, Int)], complete: Set[Char], time: Int): Int = {
      if (complete.size == steps.size) time
      else {
        val availableWorkers = workers - inProgress.size
        val started = (0 until Math.min(availableWorkers, queue.size)).map(_ => queue.dequeue()).map(c => (c, duration(c)))
        val updatedInProgress = inProgress ++ started
        val nextJob = updatedInProgress.minBy(_._2)
        val (nextToComplete, timeUntilNextEvent) = nextJob
        val newlyCompleted = complete + nextToComplete

        val toProcess = (steps -- enqueued).filter(c => dependencies(c).forall(newlyCompleted.contains))
        toProcess.foreach(queue.enqueue(_))

        val updatedProgress = (updatedInProgress - nextJob).map(p => (p._1, p._2 - timeUntilNextEvent))
        helper(enqueued ++ toProcess, updatedProgress, newlyCompleted, time + timeUntilNextEvent)
      }
    }

    val initialSteps = steps.filterNot(dependencies.contains)
    initialSteps.foreach(queue.enqueue(_))

    val result = helper(initialSteps, Set(), Set(), 0)
    println(s"Result 2: $result")
  }

  private def parse(): (Set[Char], Map[Char, Vector[Char]]) = {
    val pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin\\.".r

    val tuples = input.map {
      case pattern(upstream, downstream) => (upstream.head, downstream.head)
    }

    val steps = (tuples.map(_._1) ++ tuples.map(_._2)).toSet

    (steps, tuples.groupMap(_._2)(_._1))
  }
}

case object Day7 extends App {
  val input = Files.lines("2018/day7.txt")
  val problem = Day7(input)
  problem.solve1()
  problem.solve2()
}
