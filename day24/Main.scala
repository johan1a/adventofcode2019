import scala.io.Source
import scala.collection.mutable

object Main extends App {

  val BUG   = '#'
  val EMPTY = '.'

  val width = 5

  type Layout = Array[Array[Char]]
  type Levels = mutable.Map[Int, Array[Array[Char]]]

  assert(calculateRating(readFile("test1.txt")) == 2129920)

  var state = updateLayout(readFile("test2.txt"))
  assert(equalArrays(state, readFile("test3.txt")))
  state = updateLayout(state)
  assert(equalArrays(state, readFile("test4.txt")))
  state = updateLayout(state)
  assert(equalArrays(state, readFile("test5.txt")))
  state = updateLayout(state)
  assert(equalArrays(state, readFile("test6.txt")))

  assert(part1("test2.txt") == 2129920)

  val part1Result = part1("input.txt")
  assert(part1Result == 7543003)
  println(s"Part1: $part1Result")

  def part2(file: String): BigInt = {
    var levels: Levels = mutable.Map(0, readFile(file))
    var seen           = Set[String]()
    var minLevel       = 0
    var maxLevel       = 0
    do {
      seen = seen + makeString(layout)
      val (levels, minLevel, maxLevel) = updateLevels(layout, minLevel, maxLevel)
      if (false) {
        draw(layout)
        Thread.sleep(100)
      }
    } while (!seen.contains(makeString(levels)))
    countBugs(levels)
  }

  def updateLevels(levels: Levels, minLevel: Int, maxLevel: Int): Levels = {
//if (level >= minLevel && level <= maxLevel) {
/// Array.fill(width)(Array.fill(width)(EMPTY))
    (minLevel - 1).to(maxLevel + 1).foreach { level =>
      0.until(width).foreach { y =>
        0.until(width).foreach { x =>
          val layout = levels(level)
          val nbrAdjacent = nbrAdjacentBugs(levels, level, x, y)
          if (layout(y)(x) == BUG) {
            if (nbrAdjacent != 1) {
              EMPTY
            } else {
              BUG
            }
          } else if (layout(y)(x) == EMPTY && nbrAdjacent == 1 || nbrAdjacent == 2) {
            BUG
          } else {
            EMPTY
          }
        }
      }

    }
    var y = -1
    layout.map { line =>
      y += 1
      var x = -1
      line.map { line =>
        x += 1
      }.toArray
    }.toArray
  }

  def nbrAdjacentBugs(levels: Levels, layout: Layout, x: Int, y: Int): Int = {

  }

  def countBugs(levels: Levels): Int = {
    -1
  }

  def makeString(levels: Levels): String = {

    "TODO"
  }

  def part1(file: String): BigInt = {
    var layout: Layout = readFile(file)
    var seen           = Set[String]()
    do {
      seen = seen + layout.map(_.mkString("")).mkString("")
      layout = updateLayout(levels)
    } while (!seen.contains(layout.map(_.mkString("")).mkString("")))
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
        if (layout(y)(x) == BUG) {
          if (nbrAdjacent != 1) {
            EMPTY
          } else {
            BUG
          }
        } else if (layout(y)(x) == EMPTY && nbrAdjacent == 1 || nbrAdjacent == 2) {
          BUG
        } else {
          EMPTY
        }
      }.toArray
    }.toArray
  }

  def nbrAdjacentBugs(layout: Layout, x: Int, y: Int): Int = {
    List[(Int, Int)]((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).map { coord =>
      val x2 = coord._1
      val y2 = coord._2
      if (y2 >= 0 && x2 >= 0 && y2 < layout.size && x2 < layout(0).size) {
        if (layout(y2)(x2) == BUG) {
          1
        } else {
          0
        }
      } else {
        0
      }
    }.sum
  }

  def calculateRating(layout: Layout): BigInt = {
    var points = BigInt(1)
    var sum    = BigInt(0)
    layout.foreach { line =>
      line.foreach { elem =>
        if (elem == BUG) {
          sum += points
        }
        points = points * 2
      }
    }
    sum
  }

  def readFile(file: String): Layout = {
    val lines = Source.fromFile(file).getLines.toList
    lines.map { line =>
      line.toList.toArray
    }.toArray
  }

}
