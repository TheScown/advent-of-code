package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

case class Day4(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (values, grids, index) = parse()

    @tailrec
    def helper(
      remainingValues: Vector[Int],
      grids: Vector[Grid[(Int, Boolean)]]
    ): (Grid[(Int, Boolean)], Int) = {
      val nextValue = remainingValues.head

      @tailrec
      def gridsHelper(
        updates: Set[(Int, Complex[Int])],
        grids: Vector[Grid[(Int, Boolean)]]
      ): (Option[Grid[(Int, Boolean)]], Vector[Grid[(Int, Boolean)]]) = {
        if (updates.isEmpty) (None, grids)
        else {
          val (gridIndex, address) = updates.head
          val grid = grids(gridIndex)

          val updatedGrids = grids.updated(gridIndex, grid.updated(address, grid(address).copy(_2 = true)))
          val newGrid = updatedGrids(gridIndex)

          val row = newGrid.row(address.im)
          val column = newGrid.column(address.re)

          if (row.forall(_._2) || column.forall(_._2)) (Some(newGrid), updatedGrids)
          else gridsHelper(updates.tail, updatedGrids)
        }
      }

      val (finalGrid, updatedGrids) = gridsHelper(index(nextValue), grids)

      if (finalGrid.isDefined) (finalGrid.get, nextValue)
      else helper(remainingValues.tail, updatedGrids)
    }

    val (finalGrid, finalValue) = helper(values, grids)

    val unmarkedSum = finalGrid.filter(!_._2).map(_._1).sum
    val result = unmarkedSum * finalValue

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (values, grids, index) = parse()

    @tailrec
    def valueLoop(
      remainingValues: Vector[Int],
      grids: Vector[Grid[(Int, Boolean)]],
      winners: ListSet[Int]
    ): (Grid[(Int, Boolean)], Int) = {
      val nextValue = remainingValues.head

      @tailrec
      def updateLoop(
        updates: Set[(Int, Complex[Int])],
        grids: Vector[Grid[(Int, Boolean)]],
        winners: ListSet[Int]
      ): (Vector[Grid[(Int, Boolean)]], ListSet[Int]) = {
        if (updates.isEmpty) (grids, winners)
        else {
          val (gridIndex, address) = updates.head
          val grid = grids(gridIndex)

          val updatedGrids = grids.updated(gridIndex, grid.updated(address, grid(address).copy(_2 = true)))
          val newGrid = updatedGrids(gridIndex)

          val row = newGrid.row(address.im)
          val column = newGrid.column(address.re)

          if (!winners.contains(gridIndex) && (row.forall(_._2) || column.forall(_._2))) {
            updateLoop(updates.tail, updatedGrids, winners + gridIndex)
          }
          else updateLoop(updates.tail, updatedGrids, winners)
        }
      }

      val (updatedGrids, updatedWinners) = updateLoop(index(nextValue), grids, winners)

      if (updatedWinners.size == updatedGrids.size) (updatedGrids(updatedWinners.last), nextValue)
      else valueLoop(remainingValues.tail, updatedGrids, updatedWinners)
    }

    val (finalGrid, finalValue) = valueLoop(values, grids, ListSet())

    val unmarkedSum = finalGrid.filter(!_._2).map(_._1).sum
    val result = unmarkedSum * finalValue

    println(s"Result 2: $result")
  }

  private def parse(): (Vector[Int], Vector[Grid[(Int, Boolean)]], Map[Int, Set[(Int, Complex[Int])]]) = {
    val values = input.head.split(",").map(_.toInt).toVector
    val gridLines = input.tail.tail

    @tailrec
    def helper(
      remainingLines: Vector[String],
      grids: Vector[Grid[(Int, Boolean)]],
      map: Map[Int, Set[(Int, Complex[Int])]],
      gridIndex: Int = 0
    ): (Vector[Grid[(Int, Boolean)]], Map[Int, Set[(Int, Complex[Int])]]) = {
      val (gridLines, rest) = remainingLines.span(_.nonEmpty)

      val grid = Grid(gridLines.map(_.trim.split("\\s+").map(s => s.toInt).toVector))
        .map(v => (v, false))

      val updatedMap = grid.zipWithIndex.foldLeft(map) { case (map, ((v, _), index)) =>
        val set = map.getOrElse(v, Set())

        map + (v -> (set + (gridIndex -> index)))
      }

      val updatedGrids = grids :+ grid

      if (rest.nonEmpty) {
        helper(rest.tail, updatedGrids, updatedMap, gridIndex + 1)
      }
      else (updatedGrids, updatedMap)
    }

    val (grids, index) = helper(gridLines, Vector(), Map())

    (values, grids, index)
  }
}

case object Day4 extends App {
  val input = Files.lines("2021/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}


