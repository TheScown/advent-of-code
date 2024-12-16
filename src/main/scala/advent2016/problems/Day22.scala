package space.scown.adventofcode
package advent2016.problems

import lib._

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val nodes = parse()

    val pairs = nodes.combinations(2).toVector
    val viablePairs = pairs
      .map { case Vector(a, b) =>
        val aFitsB = a.used > 0 && a.used < b.available
        val bFitsA = b.used > 0 && b.used < a.available

        if (aFitsB && bFitsA) (a, b, 2)
        else if (aFitsB || bFitsA) (a, b, 1)
        else (a, b, 0)
      }
      .filter(_._3 > 0)

    val result = viablePairs.map(_._3).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val nodes = parse()
    val maxX = nodes.last.address.re
    val maxY = -nodes.last.address.im

    val initialGrid = Grid(nodes.grouped(maxY + 1).toVector.transpose)
    val emptyNode = initialGrid.indexWhere(_.used == 0).get
    val initialState = State(initialGrid, 0, emptyNode, Vector())
    val intermediate = intermediateAddress(nodes, initialGrid)

    val targetAddress = Complex(maxX, 0)

    val next: State => Seq[State] = {
      case state@State(grid, moves, emptyAddress, history) =>
        val emptyNode = grid(emptyAddress)
        val validNeighbours = grid.neighbours(emptyAddress).map(c => (grid.apply(c), c))
          .filter { case (node, _) => node.used <= emptyNode.size }

        val nextStates = validNeighbours
          .map { case (node, address) =>
            val updatedGrid = grid
              .updated(emptyAddress, emptyNode.moveFrom(node))
              .updated(address, node.moveFrom(emptyNode))

            State(updatedGrid, moves + 1, address, history :+ emptyAddress)
          }

        nextStates
    }

    val stateAtIntermediate = Dijkstra.solve[State](
      initialState,
      Ordering.by[State, Int](_.intermediateScore(intermediate)).reverse,
      state => state.emptyAddress == intermediate
    )(next)

    val resultState = Dijkstra.solve[State](
      stateAtIntermediate.get,
      Ordering.by[State, Int](_.score(targetAddress)).reverse,
      state => {
        state.grid(Complex.ZERO).address == targetAddress
      }
    ) (next)

    val result = resultState.get.moves

    println(s"Result 2: $result")
  }

  /**
   * Find the address at the end of the wall so we can efficiently navigate round it
   */
  private def intermediateAddress(nodes: Vector[Node], initialGrid: Grid[Node]): Complex[Int] = {
    val walls = nodes.filter {
      case Node(address, used, _, _) =>
        val neighbours = initialGrid.neighbours(address)
          .map(initialGrid.apply)
        neighbours.count(_.size < used) > 0
    }.map(_.address).toSet

    val endOfWall = walls.find { wall => initialGrid.neighbours(wall).count(w => !walls.contains(w)) == 3 }.get
    initialGrid.neighbours(endOfWall).find { ia =>
      val delta = endOfWall - ia
      walls.contains(endOfWall + delta)
    }.get
  }

  private def parse(): Vector[Node] = {
    val pattern = "/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%".r

    input.drop(2).map {
      case pattern(x, y, size, used, available) => Node(Complex(x.toInt, -y.toInt), used.toInt, available.toInt, size.toInt)
    }
  }

  case class Node(address: Complex[Int], used: Int, available: Int, size: Int) {
    def moveFrom(other: Node): Node = this.copy(
      used = other.used,
      available = size - other.used,
      address = other.address
    )
  }

  case class State(grid: Grid[Node], moves: Int, emptyAddress: Complex[Int], history: Vector[Complex[Int]]) {
    def score(targetAddress: Complex[Int]): Int = {
      val targetLocation = grid.indexWhere(_.address == targetAddress).get
      val emptyToTarget = targetLocation - emptyAddress
      // Cost is the distance to the target, plus 5 moves to get the target to the goal, plus the moves taken
      moves + (emptyToTarget.re.abs + emptyToTarget.im.abs) + 5 * (targetLocation.re.abs + targetLocation.im.abs)
    }

    def intermediateScore(targetAddress: Complex[Int]): Int = {
      val emptyToTarget = targetAddress - emptyAddress
      // Cost is the distance to the target, plus the moves taken
      moves + (emptyToTarget.re.abs + emptyToTarget.im.abs)
    }

    override def equals(obj: Any): Boolean = obj match {
      case State(g, _, _, _) => g == grid
      case _ => false
    }

    override def hashCode(): Int = grid.hashCode()
  }
}

case object Day22 extends App {
  val input = Files.lines("2016/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
