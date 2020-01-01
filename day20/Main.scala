
import io.AnsiColor._
import scala.collection.mutable
import scala.io.Source

case class Pos(x: Int, y: Int)

case class PosLevel(pos: Pos, level: Int)


object Main extends App {

  type Maze = mutable.Map[Pos, Char]

  val WALL = '#'
  val EMPTY = '.'
  val FACE = "@"


  assert(part1("test1.txt") == 23)

  assert(part1("test2.txt") == 58)

  val part1Result = part1("input.txt")
  assert(part1Result == 696)
  println(s"Part 1: ${part1Result}")

  assert(part2("test1.txt") == 26)

  assert(part2("test4.txt") == 18)

  assert(part2("test5.txt") == 21)

  assert(part2("test3.txt") == 396)

  val part2Result = part2("input.txt")
  println(s"Part 2: ${part2Result}")

  def part1(file: String): Int = {
    val (maze, portals, start, goal) = readMazeFile(file)
    shortestPath(maze, portals, start, goal)
  }

  def part2(file: String): Int = {
    val (maze, portals, start, goal) = readMazeFile(file)
    shortestPath2(maze, portals, start, goal)
  }

  def shortestPath2(maze: Maze, portals: mutable.Map[Pos, Pos], start: Pos, goal: Pos): Int = {

    val startPs = PosLevel(start, 0)
    var open = Set[PosLevel](startPs)
    val fScore = mutable.Map[PosLevel, Int](startPs -> 0).withDefaultValue(Int.MaxValue)
    val maxX = maze.keys.maxBy( p => p.x ).x
    val maxY = maze.keys.maxBy( p => p.y ).y


    while (open.nonEmpty) {
      val curr = open.minBy { n => fScore(n) }
      open = open - curr

      if (curr.pos == goal && curr.level == 0) {
        return fScore(curr)
      }

      getNeighbours(maze, portals, start, goal, curr.level, curr.pos, maxX, maxY).foreach { neighbour =>
        val tentative = fScore(curr) + 1

        val level = getNeighbourLevel(curr, neighbour, maxX, maxY)

        val psNeighbour = PosLevel(neighbour, level)


        if (tentative < fScore(psNeighbour)) {
          fScore(psNeighbour) = tentative
          open = open + psNeighbour
        }
      }
    }

    throw new RuntimeException("No route found")
  }

  def getNeighbourLevel(curr: PosLevel, neighbour: Pos, maxX: Int, maxY: Int): Int = {
    if (!adjacent(curr.pos, neighbour)) {
      if (atEdge(maxX, maxY, curr.pos)) {
        curr.level - 1
      } else {
        curr.level + 1
      }
    } else {
      curr.level
    }
  }

  def atEdge(maxX: Int, maxY: Int, pos: Pos): Boolean = {
    pos.x == 2 || pos.y == 2 || pos.x == maxX - 2 || pos.y == maxY - 2
  }

  def adjacent(a: Pos, b: Pos): Boolean = {
    Math.abs(b.x - a.x) <= 1 && Math.abs(b.y - a.y) <= 1
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
        if (maze.contains(portalPos) && maze(portalPos) == EMPTY) {
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

  def getNeighbours(maze: Maze, portals: mutable.Map[Pos, Pos], start: Pos, goal: Pos, level: Int, pos: Pos, maxX: Int, maxY: Int): Set[Pos] = {
    var realNeighbours: Set[Pos] = Set(northOf(pos), southOf(pos), westOf(pos), eastOf(pos))
      .filter { n => maze.contains(n) && maze(n) == EMPTY }
    if (level > 0) {
      realNeighbours = realNeighbours - start
      realNeighbours = realNeighbours - goal
    }
    if (portals.contains(pos) && (level > 0 || !atEdge(maxX, maxY, pos))) {
      realNeighbours + portals(pos)
    } else {
      realNeighbours
    }
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

