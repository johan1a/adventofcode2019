
import scala.io.Source
import scala.collection.mutable

object Main extends App {

  case class Pos(x: Int, y: Int)

  type Maze = mutable.Map[Pos, Char]

  val START = '@'
  val WALL = '#'
  val EMPTY = '.'

  println(part1("test1.txt"))

  // val part1Result = part1("input.txt")
  // println(s"Part 1: $part1Result")

  def part1(filename: String): Int = {
    val (maze, start) = readMazeFile(filename)
    getKeys(maze, start, Set())
  }

  def getKeys(maze: Maze, start: Pos, keys: Set[Char]): Int = {
    val reachableKeys = getReachableKeys(maze, start, keys)
    println(s"reachableKeys: $reachableKeys")
    if (reachableKeys.isEmpty) {
      return 0
    }
    val distances = reachableKeys.map { pos =>
      val dist = shortestPath(maze, start, pos, keys)
      println(s"dist: $dist")
      dist + getKeys(maze, pos, keys + maze(pos))
    }
    distances.min
  }

  def getReachableKeys(maze: Maze, start: Pos, keys: Set[Char]): Set[Pos] = {
    var checked = Set[Pos]()
    var open: Set[Pos] = Set(start)
    while (open.nonEmpty) {
      var curr = open.head
      open = open - curr
      checked = checked + curr
      open = open ++ getNeighbours(maze, curr, keys)
        .filter { n => !checked.contains(n) }
    }
    checked.filter { n => isKey(maze(n)) && !keys.contains(maze(n)) }
  }

  def canUnlock(door: Char, keys: Set[Char]): Boolean = {
    keys.find { key => keyMatches(key, door) }.nonEmpty
  }

  def shortestPath(maze: Maze, start: Pos, goal: Pos, keys: Set[Char]): Int = {
    var open = Set[Pos](start)

    var gScore = mutable.Map[Pos, Int](start -> 0).withDefaultValue(Int.MaxValue)

    while (open.nonEmpty) {
      val pos = open.minBy { p => gScore(p) + heuristic(p, goal) }
      open -= pos
      if (pos == goal) {
        return gScore(pos)
      }

      getNeighbours(maze, pos, keys).foreach { neighbour =>

        val tentative = gScore(pos) + 1
        if (tentative < gScore(neighbour)) {
          gScore(neighbour) = tentative
          open = (open + neighbour)
        }
      }
    }
    throw new RuntimeException(s"No path found between $start and $goal")
  }

  def getNeighbours(maze: Maze, pos: Pos, keys: Set[Char]): Set[Pos] = {
    Set(northOf(pos), southOf(pos), westOf(pos), eastOf(pos))
      .filter { n => maze.contains(n) && maze(n) != WALL }
      .filter { n => !isDoor(maze(n)) || canUnlock(maze(n), keys) }
  }

  def northOf(pos: Pos): Pos = {
    Pos(pos.x, pos.y + 1)
  }

  def southOf(pos: Pos): Pos = {
    Pos(pos.x, pos.y - 1)
  }

  def eastOf(pos: Pos): Pos = {
    Pos(pos.x + 1, pos.y)
  }

  def westOf(pos: Pos): Pos = {
    Pos(pos.x - 1, pos.y)
  }

  def heuristic(pos: Pos, goal: Pos): Int = {
    Math.abs(goal.y - pos.y) + Math.abs(goal.x - pos.x)
  }

  def isKey(char: Char): Boolean = {
    char >= 'a' && char <= 'z'
  }

  def isDoor(char: Char): Boolean = {
    char >= 'A' && char <= 'Z'
  }

  def keyMatches(key: Char, door: Char): Boolean = {
    key.toString.toUpperCase == door.toString
  }

  def readMazeFile(mazeFile: String): (Maze, Pos) = {
    var y = 0
    var start = Pos(0,0)
    val maze = mutable.Map[Pos, Char]()
    Source.fromFile(mazeFile).getLines.foreach { line =>
      0.until(line.size).foreach { x =>
        val pos = Pos(x, y)
        val char = line.charAt(x)
        maze(pos) = char
        if (char == START) {
          start = pos
        }
      }
      y += 1
    }
    (maze, start)
  }

}
