package space.scown.advent2023
package lib

import java.util.concurrent.TimeUnit

object Timer {

  def time(f: () => Unit): Unit = {
    val start = System.nanoTime()
    f()
    val end = System.nanoTime()
    val duration = TimeUnit.MICROSECONDS.convert(end - start, TimeUnit.NANOSECONDS)
    println(s"Duration: $duration")
  }

}
