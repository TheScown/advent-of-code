package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

case class Day7(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val map = parse()

    def helper(bag: String, cache: Map[String, Boolean]): Map[String, Boolean] = {
      if (cache.contains(bag)) cache
      else {
        val counts = map(bag)

        if (counts.isEmpty) cache + (bag -> false)
        else if (counts.contains("shiny gold")) cache + (bag -> true)
        else {
          val finalCache = counts.foldLeft(cache) { case (cache, (bag, _)) => helper(bag, cache) }

          val result = counts.exists(p => finalCache(p._1))

          finalCache + (bag -> result)
        }
      }
    }

    val finalCache = map.keys.foldLeft(Map[String, Boolean]()) { (cache, bag) =>
      helper(bag, cache)
    }

    val result = finalCache.count(_._2)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val map = parse()

    def helper(bag: String, cache: Map[String, Int]): Map[String, Int] = {
      if (cache.contains(bag)) cache
      else {
        val counts = map(bag)

        if (counts.isEmpty) cache + (bag -> 0)
        else {
          val finalCache = counts.foldLeft(cache) { case (cache, (bag, _)) => helper(bag, cache) }
          val result = counts.foldLeft(0) { case (total, (bag, count)) =>
            total + count * (finalCache(bag) + 1)
          }

          finalCache + (bag -> result)
        }
      }
    }

    val finalCache = helper("shiny gold", Map())
    val result = finalCache("shiny gold")

    println(s"Result 2: $result")
  }

  private def parse(): Map[String, Map[String, Int]] = {
    val noOtherBags = "([a-z ]+) bags contain no other bags.".r
    val bagsWithBags = "([a-z ]+) bags contain (\\d.*?).".r
    val bagInnerPattern = "(\\d+) ([a-z ]+) bags?".r

    input.foldLeft(Map[String, Map[String, Int]]()) { (map, line) =>
      val entry = line match {
        case noOtherBags(name) => name -> Map[String, Int]()
        case bagsWithBags(name, inner) =>
          val nested = inner.split(", ").foldLeft(Map[String, Int]()) { (map, line) =>
            val newEntry = line match {
              case bagInnerPattern(count, name) => name -> count.toInt
            }

            map + newEntry
          }

          (name -> nested)
      }

      map + entry
    }
  }
}

case object Day7 extends App {
  val input = Files.lines("2020/day7.txt")
  val problem = Day7(input)
  problem.solve1()
  problem.solve2()
}
