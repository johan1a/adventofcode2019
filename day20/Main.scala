
import scala.collection.mutable
import scala.io.Source

object Main extends App {

  case class Pos(x: Int, y: Int)

  type Maze = mutable.Map[Pos, Char]

  val WALL = '#'
  val EMPTY = '.'

  assert(part1("test1.txt") == 23)

  assert(part1("test2.txt") == 58)

  val part1Result = part1("input.txt")
  assert(part1Result == 696)
  println(s"Part 1: ${part1Result}")

  def part1(file: String): Int = {
    val (maze, portals, start, goal) = readMazeFile(file)
    shortestPath(maze, portals, start, goal)
  }

  def shortestPath(maze: Maze, portals: mutable.Map[Pos, Pos], start: Pos, goal: Pos): Int = {

    var open = Set[Pos](start)
    val fScore = mutable.Map[Pos, Int](start -> 0).withDefaultValue(Int.MaxValue)

    while (open.nonEmpty) {
      val curr = open.minBy { n => fScore(n) }
      open = open - curr

      if (curr == goal) {
        return fScore(goal)
      }

      getNeighbours(maze, portals, curr).foreach { neighbour =>

        val tentative = fScore(curr) + 1

        if (tentative < fScore(neighbour)) {
          fScore(neighbour) = tentative
          open = open + neighbour
        }
      }
    }

    throw new RuntimeException("No route found")
  }

  def readMazeFile(mazeFile: String): (Maze, mutable.Map[Pos, Pos], Pos, Pos) = {
    var maxX = 0
    var y = 0
    var starts = Set[Pos]()
    val maze = mutable.Map[Pos, Char]()
    Source.fromFile(mazeFile).getLines.foreach { line =>
      0.until(line.size).foreach { x =>
        val pos = Pos(x, y)
        val char = line.charAt(x)
        maxX = Math.max(x, maxX)
        if (char != ' ') {
          maze(pos) = char
        }
      }
      y += 1
    }
    val maxY = y - 1
    val (portals, start, goal) = parsePortals(maze, maxX, maxY)
    (maze, portals, start, goal)
  }

  def parsePortals(maze: Maze, maxX: Int, maxY: Int): (mutable.Map[Pos, Pos], Pos, Pos) = {
    val lettersToPos = mutable.Map[String, List[Pos]]().withDefaultValue(List())
    var start = Pos(-1, -1)
    var goal = Pos(-1, -1)
    maze.keys.foreach { pos =>
      val char = maze(pos)
      if (isLetter(char)) {
        var portalPos = pos
        var otherLetter = char
        var portalLetters: String = ""
        if (maze.contains(northOf(pos)) && isLetter(maze(northOf(pos)))) {
          portalPos = southOf(pos)
          otherLetter = maze(northOf(pos))
          portalLetters = s"${otherLetter}${char}"
        } else if (maze.contains(eastOf(pos)) && isLetter(maze(eastOf(pos)))) {
          portalPos = westOf(pos)
          otherLetter = maze(eastOf(pos))
          portalLetters = s"${char}${otherLetter}"
        } else if (maze.contains(westOf(pos)) && isLetter(maze(westOf(pos)))) {
          portalPos = eastOf(pos)
          otherLetter = maze(westOf(pos))
          portalLetters = s"${otherLetter}${char}"
        } else if (maze.contains(southOf(pos)) && isLetter(maze(southOf(pos)))) {
          portalPos = northOf(pos)
          otherLetter = maze(southOf(pos))
          portalLetters = s"${char}${otherLetter}"
        }
        if (maze.contains(portalPos)) {
          if (portalLetters == "AA") {
            start = portalPos
          } else if (portalLetters == "ZZ") {
            goal = portalPos
          } else {
            lettersToPos(portalLetters) = portalPos +: lettersToPos(portalLetters)
          }
        }
      }
    }
    val portalMap = mutable.Map[Pos, Pos]()
    lettersToPos.values.foreach { positions =>
      val a = positions.head
      val b = positions.last
      portalMap(a) = b
      portalMap(b) = a
    }
    (portalMap, start, goal)
  }

  def isLetter(char: Char): Boolean = {
    char >= 'A' && char <= 'Z'
  }

  def getNeighbours(maze: Maze, portals: mutable.Map[Pos, Pos], pos: Pos): Set[Pos] = {
    val realNeighbours: Set[Pos] = Set(northOf(pos), southOf(pos), westOf(pos), eastOf(pos))
      .filter { n => maze.contains(n) && maze(n) == EMPTY }
    if (portals.contains(pos)) {
      realNeighbours + portals(pos)
    } else {
      realNeighbours
    }
  }

  def northOf(pos: Pos): Pos = {
    Pos(pos.x, pos.y - 1)
  }

  def southOf(pos: Pos): Pos = {
    Pos(pos.x, pos.y + 1)
  }

  def eastOf(pos: Pos): Pos = {
    Pos(pos.x + 1, pos.y)
  }

  def westOf(pos: Pos): Pos = {
    Pos(pos.x - 1, pos.y)
  }


}

