package space.scown.adventofcode
package advent2023.problems.day20

sealed trait Module {
  val name: String
  val destinations: List[String]

  def apply(modules: Map[String, Module], input: (String, Boolean)): (Module, Seq[(String, String, Boolean)])
}

case class FlipFlop(name: String, destinations: List[String], on: Boolean) extends Module {

  override def apply(modules: Map[String, Module], input: (String, Boolean)): (Module, Seq[(String, String, Boolean)]) = {
    if (input._2) (this, List())
    else {
      if (on) {
        (this.copy(on = false), destinations.map {(name, _, false)})
      } else {
        (this.copy(on = true), destinations.map {(name, _, true)})
      }
    }
  }
}

case class Conjunction(name: String, destinations: List[String], memory: Map[String, Boolean]) extends Module {
  override def apply(modules: Map[String, Module], input: (String, Boolean)): (Module, Seq[(String, String, Boolean)]) = {
    val newMemory = memory + (input._1 -> input._2)

    if (newMemory.values.forall(_.self)) {
      (this.copy(memory = newMemory), destinations.map {(name, _, false)})
    }
    else {
      (this.copy(memory = newMemory), destinations.map {(name, _, true)})
    }
  }
}

case class Broadcaster(destinations: List[String]) extends Module {
  override val name: String = "broadcaster"

  override def apply(modules: Map[String, Module], input: (String, Boolean)): (Module, Seq[(String, String, Boolean)]) = {
    (this, destinations.map {(name, _, input._2)})
  }
}

case class Dummy(name: String) extends Module {

  override val destinations: List[String] = Nil

  override def apply(modules: Map[String, Module], input: (String, Boolean)): (Module, Seq[(String, String, Boolean)]) = {
    (this, Nil)
  }

}
