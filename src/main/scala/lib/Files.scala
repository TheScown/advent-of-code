package space.scown.advent2023
package lib

import scala.io.Source
import scala.util.parsing.input.PagedSeq

object Files {
  def lines(filename: String): Vector[String] = {
    Source.fromResource(filename).getLines().toVector
  }

  def pagedSequence(filename: String): PagedSeq[Char] = {
    PagedSeq.fromSource(Source.fromResource(filename))
  }

  def grid(filename: String, delimiterPattern: String = ""): Vector[Vector[Char]] = {
    Files.lines(filename).map(l => l.split(delimiterPattern).map(_.head).toVector)
  }
}
