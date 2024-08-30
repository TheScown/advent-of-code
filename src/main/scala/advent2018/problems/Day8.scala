package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

case class Day8(input: String) extends Problem {
  override def solve1(): Unit = {
    val tree = parse()

    def helper(tree: List[Int]): (List[Int], Int) = {
      tree match {
        case children :: metadata :: rest =>
          val (remaining, sum) = (0 until children).foldLeft((rest, 0)){(acc, _) =>
            val (rest, sum) = helper(acc._1)
            (rest, acc._2 + sum)
          }
          val (myMetadata, restOfTree) = remaining.splitAt(metadata)
          (restOfTree, sum + myMetadata.sum)
      }
    }

    val (_, result) = helper(tree)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val tree = parse()

    def helper(tree: List[Int]): (List[Int], Int) = {
      tree match {
        case children :: metadata :: rest =>
          if (children == 0) {
            val (myMetadata, restOfTree) = rest.splitAt(metadata)
            (restOfTree, myMetadata.sum)
          }
          else {
            val (remaining, childNodes) = (0 until children).foldLeft((rest, Vector[Int]())) { (acc, _) =>
              val (restOfList, childValue) = helper(acc._1)
              (restOfList, acc._2 :+ childValue)
            }
            val (myMetadata, restOfTree) = remaining.splitAt(metadata)
            (restOfTree, myMetadata.map(m => if (m > childNodes.size) 0 else childNodes(m - 1)).sum)
          }
      }
    }

    val (_, result) = helper(tree)

    println(s"Result 2: $result")
  }

  private def parse(): List[Int] = {
    input.split("\\s").map(_.toInt).toList
  }
}

case object Day8 extends App {
  val input = Files.lines("2018/day8.txt").head
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
