package space.scown.advent2023
package lib

import scala.io.Source

object Files {
  def lines(filename: String): Vector[String] = {
    Source.fromResource(filename).getLines().toVector
  }

}
