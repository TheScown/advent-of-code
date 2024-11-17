package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Grid, Gui, Problem}

import java.awt.Color
import java.awt.image.BufferedImage

case class Day8(input: String) extends Problem {
  private val WIDTH = 25
  private val HEIGHT = 6

  override def solve1(): Unit = {
    val layers = input.toVector.grouped(WIDTH * HEIGHT)
    val minLayer = layers.minBy(_.count(_ == '0'))
    val result = minLayer.count(_ == '1') * minLayer.count(_ == '2')

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val layers = input.toVector.grouped(WIDTH * HEIGHT).toVector
    val gridLayers = layers.map(_.grouped(WIDTH).toVector)
    val grids = gridLayers.map(Grid(_))

    val finalGrid = grids.reduce { (acc, nextGrid) =>
      acc.zipWithIndex.map { case (value, index) =>
        if (value == '2') nextGrid(index)
        else value
      }
    }

    val rgbGrid = finalGrid.map {c =>
      if (c == '0') Some(Color.BLACK.getRGB)
      else if (c == '1') Some(Color.WHITE.getRGB)
      else None
    }

    val image = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)

    rgbGrid.indices.foreach { i =>
      rgbGrid(i) match {
        case Some(rgb) => image.setRGB(i.re, -i.im, rgb)
        case None =>
      }
    }

    Gui.renderImage(image)
  }
}

case object Day8 extends App {
  val input = Files.lines("2019/day8.txt").head
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
