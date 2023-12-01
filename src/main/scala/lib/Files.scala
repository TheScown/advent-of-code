package space.scown.advent2023
package lib

import scala.io.Source

object Files {
  def lines(filename: String): Iterator[String] = {
    Source.fromResource(filename).getLines()
  }
  
}
