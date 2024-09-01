package space.scown.adventofcode
package advent2018.problems

import lib._

import java.awt.Color
import java.awt.image.BufferedImage

case class Day10(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val points = parse()
    val startTime = 10304
    val endTime = 10305

    (startTime until endTime).foreach { t =>
      val pointsAtT = points.map(_.at(t))
      val minX = pointsAtT.minBy(_.x1).x1
      val minY = pointsAtT.minBy(_.x2).x2
      val origin = Vec2(minX, minY)

      val adjusted  = pointsAtT.map(_ - origin)
      val maxX = adjusted.maxBy(_.x1).x1 + 1
      val maxY = adjusted.maxBy(_.x2).x2 + 1

      if (maxX < 100 && maxY < 100) {
        val initialGrid = Grid.of(maxY, maxX, '.')
        val grid = adjusted.foldLeft(initialGrid) { (grid, point) =>
          grid.updated(Complex(point.x1, -point.x2), '#')
        }

        val image = grid.zipWithIndex.foldLeft(new BufferedImage(maxX, maxY, BufferedImage.TYPE_INT_RGB)) { (image, item) =>
          val (value, index) = item
          val colour = if (value == '#') Color.BLACK else Color.WHITE
          image.setRGB(index.re.toInt, -index.im.toInt, colour.getRGB)

          image
        }

        Gui.renderImage(image)
      }
    }

    println("Result 1: See window")
  }

  override def solve2(): Unit = {
    println(s"Result 2: 10304")
  }

  private def parse(): Vector[Point] = {
    val pattern = "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>".r

    input.map {
      case pattern(px, py, vx, vy) => Point(Vec2(px.toInt, py.toInt), Vec2(vx.toInt, vy.toInt))
    }
  }

  private case class Point(p: Vec2[Int], v: Vec2[Int]) {
    def at(t: Int): Vec2[Int] = p + t *: v
  }
}

case object Day10 extends App {
  val input = Files.lines("2018/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
