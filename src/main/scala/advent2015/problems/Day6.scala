package space.scown.adventofcode
package advent2015.problems

import lib.{Complex, Files, Problem, Timer}

case class Day6(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val result = input.foldLeft(Set[Complex[Int]]()) { (set, line) =>
      parse(line) match {
        case On(start, end) =>
          set ++ (start to end)
        case Off(start, end) =>
          set -- (start to end)
        case Toggle(start, end) =>
          (start to end).foldLeft(set) { (set, z) =>
          if (set.contains(z)) set - z
          else set + z
        }
      }
    }.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = input.foldLeft(Map[Complex[Int], Int]()) { (map, line) =>
      parse(line) match {
        case On(start, end) =>
          (start to end).foldLeft(map) { (map, z) =>
            if (map.contains(z)) map + (z -> (map(z) + 1))
            else map + (z -> 1)
          }
        case Off(start, end) =>
          (start to end).foldLeft(map) { (map, z) =>
            if (map.contains(z) && map(z) > 0) map + (z -> (map(z) - 1))
            else map
          }
        case Toggle(start, end) =>
          (start to end).foldLeft(map) { (map, z) =>
            if (map.contains(z)) map + (z -> (map(z) + 2))
            else map + (z -> 2)
          }
      }
    }.foldLeft(0) { (sum, entry) => entry match {
      case (_, brightness) => sum + brightness
    } }

    println(s"Result 2: $result")
  }

  private def parse(line: String): Operation = {
    val pattern = "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)".r
    val matches = pattern.findFirstMatchIn(line).get

    val start = Complex(matches.group(2).toInt, matches.group(3).toInt)
    val end = Complex(matches.group(4).toInt, matches.group(5).toInt)

    matches.group(1) match {
      case "turn on" => On(start, end)
      case "turn off" => Off(start, end)
      case "toggle" => Toggle(start, end)
    }
  }


  private sealed trait Operation {
    def start: Complex[Int]
    def end: Complex[Int]
  }
  private case class On(start: Complex[Int], end: Complex[Int]) extends Operation
  private case class Off(start: Complex[Int], end: Complex[Int]) extends Operation
  private case class Toggle(start: Complex[Int], end: Complex[Int]) extends Operation

}

case object Day6 extends App {
  val input = Files.lines("2015/day6.txt")
  val problem = Day6(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
