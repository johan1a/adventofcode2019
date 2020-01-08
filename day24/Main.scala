import scala.io.Source
import scala.collection.mutable

object Main extends App {

  val BUG   = '#'
  val EMPTY = '.'

  val width = 5

  type Layout = Array[Array[Char]]
  type Levels = mutable.Map[Int, Layout]

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
    var levels: Levels = mutable.Map(0 -> readFile(file))
    var seen           = Set[String]()
    var minLevel       = 0
    var maxLevel       = 0
    do {
      seen = seen + makeString(levels)
      val (levels2, minLevel2, maxLevel2) = updateLevels(levels, minLevel, maxLevel)
      levels = levels2
      minLevel = minLevel2
      maxLevel = maxLevel2
    } while (!seen.contains(makeString(levels)))
    countBugs(levels)
  }

  def emptyLevel(): Layout = {
    Array.fill(width)(Array.fill(width)(EMPTY))
  }

  def updateLevels(levels: Levels, minLevel: Int, maxLevel: Int): (Levels, Int, Int) = {
    var newMinLevel = Int.MinValue
    var newMaxLevel = minLevel
    val newLevels = mutable.Map[Int, Layout]()

    (minLevel - 1).to(maxLevel + 1).map { level =>
      var containsBugs = false
      val layout       = levels(level)
      val outerLevel = if (level == minLevel) {
        emptyLevel()
      } else {
        levels(level - 1)
      }
      val innerLevel = if (level == maxLevel) {
        emptyLevel()
      } else {
        levels(level + 1)
      }
      val newLevel = 0.until(width).map { y =>
          0.until(width).map { x =>
              val nbrAdjacent = nbrAdjacentBugs(layout, outerLevel, innerLevel, x, y)
              if (layout(y)(x) == BUG) {
                if (nbrAdjacent != 1) {
                  EMPTY
                } else {
                  containsBugs = true
                  BUG
                }
              } else if (layout(y)(x) == EMPTY && nbrAdjacent == 1 || nbrAdjacent == 2) {
                containsBugs = true
                BUG
              } else {
                EMPTY
              }
            }.toArray
        }.toArray
      if (containsBugs) {
        if (minLevel == Long.MinValue) {
          newMinLevel = level
        }
        newMaxLevel = level
      }
      if(level >= newMinLevel && level <= newMaxLevel){
        newLevels(level) = newLevel
      }
    }
    (newLevels, newMinLevel, newMaxLevel)
  }

  def countBug(layout: Layout, x: Int, y: Int): Int = {
    if (layout(y)(x) == BUG) {
      1
    } else {
      0
    }
  }

  // Yikes!
  def nbrAdjacentBugs(layout: Layout, outerLevel: Layout, innerLevel: Layout, x: Int, y: Int): Int = {
    var sum = 0
    if (x == 0) {
      if (y == 0) {
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 1, 2)
        sum += countBug(outerLevel, 2, 1)
      } else if (y >= 1 && y <= 3) {
        sum += countBug(layout, x, y - 1)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 1, 2)
      } else if (y == width - 1) {
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 1, 2)
        sum += countBug(outerLevel, 2, 3)
      } else {
        throw new RuntimeException("ERROR")
      }
    } else if (x == 1) {
      if (y == 0) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 2, 1)
      } else if (y == 1 || y == 3) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(layout, x, y + 1)
      } else if (y == 2) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(layout, x, y + 1)
        0.until(width).foreach { y =>
          sum += countBug(innerLevel, 0, y)
        }
      } else if (y == width - 1) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(outerLevel, 2, 3)
      } else {
        throw new RuntimeException("ERROR")
      }
    } else if (x == 2) {
      if (y == 0) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 2, 1)
      } else if (y == 1) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y - 1)
        0.until(width).foreach { x =>
          sum += countBug(innerLevel, x, 0)
        }
      } else if (y == 2) {
        // No-op
      } else if (y == width - 2) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        0.until(width).foreach { x =>
          sum += countBug(innerLevel, x, 4)
        }
      } else if (y == width - 1) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(outerLevel, 2, 3)
      } else {
        throw new RuntimeException("ERROR")
      }
    } else if (x == width - 2) {
      if (y == 0) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 2, 1)
      } else if (y == 1 || y == 3) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(layout, x, y + 1)
      } else if (y == 2) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(layout, x, y + 1)
        0.until(width).foreach { y =>
          sum += countBug(innerLevel, width - 1, y)
        }
      } else if (y == width - 1) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x + 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(outerLevel, 2, 3)
      } else {
        throw new RuntimeException("ERROR")
      }
    } else if (x == width - 1) {
      if (y == 0) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(outerLevel, 2, 1)
        sum += countBug(outerLevel, 3, 2)
      } else if (y == 1 && x <= 3) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x, y + 1)
        sum += countBug(layout, x, y - 1)
        sum += countBug(outerLevel, 3, 2)
      } else if (y == width - 1) {
        sum += countBug(layout, x - 1, y)
        sum += countBug(layout, x, y - 1)
        sum += countBug(outerLevel, 3, 2)
        sum += countBug(outerLevel, 2, 3)
      } else {
        throw new RuntimeException("ERROR")
      }
    }
    sum
  }

  def countBugs(levels: Levels): Int = {
    levels.map { entry =>
      val layout = entry._2
      layout.map { line =>
        line.count(_ == BUG)
      }.sum
    }.sum
  }

  def makeString(levels: Levels): String = {
    levels.map { entry =>
        val layout = entry._2
        layout
          .map { line =>
            line.mkString("")
          }
          .mkString("")
      }
      .mkString("")
  }

  def part1(file: String): BigInt = {
    var layout: Layout = readFile(file)
    var seen           = Set[String]()
    do {
      seen = seen + layout.map(_.mkString("")).mkString("")
      layout = updateLayout(layout)
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
