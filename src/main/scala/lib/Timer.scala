package space.scown.advent2023
package lib

import java.util.concurrent.TimeUnit

object Timer {

  def time(f: () => Unit): Unit = {
    val start = System.nanoTime()
    f()
    val end = System.nanoTime()
    val duration = end - start

    if (duration < 1000) {
      println(s"Duration: $duration ns")
    } else if (duration < 1_000_000) {
      println(s"Duration: ${TimeUnit.MICROSECONDS.convert(duration, TimeUnit.NANOSECONDS)} µs")
    } else if (duration < 5_000_000_000L) {
      println(s"Duration: ${TimeUnit.MILLISECONDS.convert(duration, TimeUnit.NANOSECONDS)} ms")
    } else {
      println(s"Duration: ${TimeUnit.SECONDS.convert(duration, TimeUnit.NANOSECONDS)} s")
    }


  }

}
