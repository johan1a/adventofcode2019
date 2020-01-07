
import scala.io.Source

object Main extends App {

  val BUG = '#'
  val EMPTY = '.'

  type Layout = Array[Array[Char]]

  println(part1("input.txt"))

  def part1(file: String): BigInt = {
    var layout: Layout = readFile(file)
    var prevLayout = layout
    do {
      prevLayout = layout
      layout = updateLayout(prevLayout)
      if (true) {
        draw(layout)
        Thread.sleep(300)
      }
    } while (!(equalArrays(layout, prevLayout)))
    calculateRating(layout)
  }

  def equalArrays(a: Layout, b: Layout): Boolean = {
    a.indices.foreach { i =>
      a(i).indices.foreach { j =>
        if (a(i)(j) != (b(i)(j))) {
          return false
        }
      }
    }
    true
  }

  def draw(layout: Layout): Unit = {
    println("\n")
    layout.foreach { line =>
      println(line.mkString(""))
    }
  }

  def updateLayout(layout: Layout): Layout = {
    var y = -1
    layout.map { line =>
      y += 1
      var x = -1
      line.map { line =>
        x += 1
        val nbrAdjacent = nbrAdjacentBugs(layout, x, y)
        if (layout(y)(x) == BUG && nbrAdjacent == 1) {
          EMPTY
        } else if (layout(y)(x) == EMPTY && nbrAdjacent == 1 || nbrAdjacent == 2) {
          BUG
        } else {
          layout(y)(x)
        }
      }.toArray
    }.toArray
  }

  def nbrAdjacentBugs(layout: Layout, x: Int, y: Int): Int = {
    val res = (y - 1).to(y + 1).map { y2 =>
      (x - 1).to(x + 1).map { x2 =>
        if (y2 != y && x2 != x2 && y2 >= 0 && x2 >= 0 && y2 < layout.size && x2 < layout(0).size) {
          if (layout(y)(x) == BUG) {
            1
          } else {
            0
          }
        } else {
          0
        }
      }.sum
    }.sum
    println(res)
    res
  }

  def calculateRating(layout: Layout): BigInt = {
    BigInt(-1)
  }

  def readFile(file: String): Layout = {
    val lines = Source.fromFile(file).getLines.toList
    lines.map { line =>
      line.toList.toArray
    }.toArray
  }

}
