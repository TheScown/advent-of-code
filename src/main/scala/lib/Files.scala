package space.scown.adventofcode2019
package lib

import scala.io.Source

object Files {
  def lines(filename: String): Vector[String] = {
    Source.fromResource(filename).getLines().toVector
  }

}
