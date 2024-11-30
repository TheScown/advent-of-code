package space.scown.adventofcode
package advent2019.intcode

case class AsciiAdapter(output: Output) {

  def outputs: Seq[Long] = output.outputs

  def stringOutput: String = output.outputs.map(_.toChar).mkString("")

  def sendString(input: String): AsciiAdapter = output match {
    case RequiresInput(_, _) => input.map(_.toLong).foldLeft(this) { (ascii, c) =>
      ascii.output match {
        case RequiresInput(_, continue) => AsciiAdapter(continue(c))
        case t@Termination(_, _) => AsciiAdapter(t)
      }
    }
    case t@Termination(_, _) =>
      print(AsciiAdapter(t).stringOutput)
      throw new IllegalStateException("Sending to terminated computer")
  }

}
