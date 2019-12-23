import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object Main extends App {

  val ADD = 1
  val MUL = 2
  val INPUT = 3
  val OUTPUT = 4
  val JNZ = 5
  val JEZ = 6
  val LESS_THAN = 7
  val EQUALS = 8
  val RELATIVE_BASE = 9
  val EXIT = 99

  val HIT_WALL = 0
  val MOVED = 1
  val FOUND_OXYGEN = 2

  val WALL = "#"
  val EMPTY = "."
  val PLAYER = "D"
  val OXYGEN = "O"
  val START = "S"

  val NORTH = 1
  val SOUTH = 2
  val WEST = 3
  val EAST = 4

  case class ComputerState(instructions: mutable.Map[BigInt, BigInt],
                           var sp: BigInt = 0,
                           var inputs: List[BigInt] = List(),
                           var outputs: List[BigInt] = List(),
                           var relativeBase: BigInt = 0,
                           var halted: Boolean = false)

  case class Pos(x: Int, y: Int)

  val part1Result = part1("maze.txt")
  assert(part1Result == 424)
  println(s"Part 1: ${part1Result}")

  discover("input.txt")

  def part1(mazeFile: String): Int = {
    val (maze, start, goal) = readMazeFile(mazeFile)
    println(start)
    println(goal)
    var open = Set[Pos](start)
    val gscore = mutable.Map[Pos, Int]().withDefaultValue(Int.MaxValue)
    gscore(start) = 0

    while (open.nonEmpty) {
      val node = open.minBy { n => gscore(n) + heuristic(n, goal) }
      open -= node
      if (node == goal) {
        println("found it ")
        return gscore(node)
      }
      val neighbours = Set(northOf(node), southOf(node), westOf(node), eastOf(node))
        .filter { n =>
          maze.contains(n) && maze(n) != WALL
        }
      neighbours.foreach { neighbour =>
        val tentative = gscore(node) + 1
        if (tentative < gscore(neighbour)) {
          gscore(neighbour) = tentative
          if (!open.contains(neighbour)) {
            open = open ++ Set(neighbour)
          }
        }

      }
    }
    -1
  }


  def heuristic(pos: Pos, goal: Pos): Int = {
    Math.abs(goal.y - pos.y) + Math.abs(goal.x - pos.x)
  }

  def readMazeFile(mazeFile: String): (mutable.Map[Pos, String], Pos, Pos) = {
    var y = 0
    var start = Pos(0,0)
    var goal = Pos(0,0)
    val maze = mutable.Map[Pos, String]()
    Source.fromFile(mazeFile).getLines.foreach { line =>
      0.until(line.size).foreach { x =>
        val pos = Pos(x, y)
        val char = line.charAt(x).toString
        maze(pos) = char
        if (char == START) {
          start = pos
        } else if (char == OXYGEN) {
          goal = pos
        }
      }
      y += 1
    }
    (maze, start, goal)
  }

  def discover(program: String): Unit = {
    var state = ComputerState(readFile(program))

    var pos = Pos(0, 0)
    var map = mutable.Map[Pos, String](pos -> START)
    var path = mutable.Queue[Int]()
    var prevMove = -1
    val potential = mutable.Set[Pos](pos)
    val visited = mutable.Set[Pos]()
    val familiarity = mutable.Map[Pos, Int]().withDefaultValue(0)

    while (!state.halted && potential.nonEmpty) {
      state = runProgram(state)
      if (state.outputs.nonEmpty) {
        val status = state.outputs.head.toInt
        state.outputs = List()

        if (prevMove != -1) {
          pos = updatePos(pos, prevMove, status)
          updateMap(map, visited, potential, pos, prevMove, status)
        }
      }
      familiarity(pos) = familiarity(pos) + 1
      addToPotential(visited, potential, pos)
      draw(map, pos)
      val nextMove: Int = getNextMove(map, visited, potential, familiarity, pos)

      state.inputs = List(nextMove)
      prevMove = nextMove
      Thread.sleep(100)
    }
  }

  def strMove(move: Int): String = {
    move match {
      case NORTH => "NORTH"
      case SOUTH => "SOUTH"
      case EAST => "EAST"
      case WEST => "WEST"
    }
  }

  def addToPotential(visited: mutable.Set[Pos], potential: mutable.Set[Pos], pos: Pos): Unit = {
    visited += pos
    potential -= pos

    if (!visited.contains(northOf(pos))) {
      potential += northOf(pos)
    }
    if (!visited.contains(southOf(pos))) {
      potential += southOf(pos)
    }
    if (!visited.contains(westOf(pos))) {
      potential += westOf(pos)
    }
    if (!visited.contains(eastOf(pos))) {
      potential += eastOf(pos)
    }
  }

  def draw(map: mutable.Map[Pos, String], playerPos: Pos): Unit = {
    var xx = List[Int]()
    var yy = List[Int]()
    map.keys.foreach { pos =>
      xx = xx ++ Set(pos.x)
      yy = yy ++ Set(pos.y)
    }
    val minX = xx.min
    val maxX = xx.max
    val minY = yy.min
    val maxY = yy.max
    println("----------")
    minY.to(maxY).foreach { y =>
      minX.to(maxX).foreach { x =>
        val thepos = Pos(x, y)
        if (thepos == playerPos) {
          print(PLAYER)
        } else if (map.contains(thepos)) {
          print(map(thepos))
        } else {
          print(" ")
        }
      }
      println()
    }

  }

  def updatePos(pos: Pos, prevMove: Int, status: Int): Pos = {
    if (status == HIT_WALL) {
      return pos
    }
    prevMove match {
        case NORTH => northOf(pos)
        case SOUTH => southOf(pos)
        case WEST => westOf(pos)
        case EAST => eastOf(pos)
    }
  }

  def updateMap(map: mutable.Map[Pos, String], visited: mutable.Set[Pos], potential: mutable.Set[Pos], pos: Pos, prevMove: Int, status: Int): Unit = {
    status match {
      case HIT_WALL => {
        prevMove match {
          case NORTH => {
            map(northOf(pos)) = WALL
            visited += northOf(pos)
            potential -= northOf(pos)
          }
          case SOUTH => {
            map(southOf(pos)) = WALL
            visited += southOf(pos)
            potential -= southOf(pos)
          }
          case WEST => {
            map(westOf(pos)) = WALL
            visited += westOf(pos)
            potential -= westOf(pos)
          }
          case EAST => {
            map(eastOf(pos)) = WALL
            visited += eastOf(pos)
            potential -= eastOf(pos)
          }
        }
      }
      case MOVED => map(pos) = EMPTY
      case FOUND_OXYGEN => map(pos) = OXYGEN
    }
  }

  def getNextMove(map: mutable.Map[Pos, String], visited: mutable.Set[Pos], potential: mutable.Set[Pos], familiarity: mutable.Map[Pos, Int], pos: Pos): Int = {
    val possible = getPossibleNextPositions(map, pos)
    val nextpos = possible.find { p =>
      potential.contains(p)
    }.orElse {
      Some(possible.minBy { familiarity(_) })
    }.get
    if (nextpos == northOf(pos)) {
      NORTH
    } else if (nextpos == southOf(pos)) {
      SOUTH
    } else if (nextpos == westOf(pos)) {
      WEST
    } else {
      EAST
    }
  }

  def getPossibleNextPositions(map: mutable.Map[Pos, String], pos: Pos): List[Pos] = {
    val north = northOf(pos)
    val south = southOf(pos)
    val west = westOf(pos)
    val east = eastOf(pos)
    var moves = List[Pos]()
    if (isPossibleMove(map, north)) {
      moves = north +: moves
    }
    if (isPossibleMove(map, south)) {
      moves = south +: moves
    }
    if (isPossibleMove(map, west)) {
      moves = west +: moves
    }
    if (isPossibleMove(map, east)) {
      moves = east +: moves
    }
    moves
  }

  def isPossibleMove(map: mutable.Map[Pos, String], pos: Pos): Boolean = {
    !map.contains(pos) || map(pos) == EMPTY
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

  def readFile(filename: String): mutable.Map[BigInt, BigInt] = {
    val m = mutable.Map[BigInt, BigInt]().withDefaultValue(BigInt(0))
    val file = Source.fromFile(filename)
    file.getLines.foreach { line =>
      line.split(",").zipWithIndex.foreach { (intAndIndex: (String, Int)) =>
        m(intAndIndex._2) = BigInt(intAndIndex._1.toLong)
      }
    }
    file.close()
    m
  }

  def add(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a + b)
    state.sp += nbrSteps(ADD)
  }

  def mul(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a * b)
    state.sp += nbrSteps(MUL)
  }

  def input(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = state.inputs.head
    state.inputs = state.inputs.tail
    setVal(state.instructions, state.sp + 1, paramModes.head, relativeBase, a)
    state.sp += nbrSteps(INPUT)
  }

  def output(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    state.sp += nbrSteps(OUTPUT)
    state.outputs = state.outputs :+ a
  }

  def jnz(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a != 0) {
      state.sp = b
    } else {
      state.sp += nbrSteps(JNZ)
    }
  }

  def jez(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a == 0) {
      state.sp = b
    } else {
      state.sp += nbrSteps(JEZ)
    }
  }

  def lessThan(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a < b) {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 1)
    } else {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 0)
    }
    state.sp += nbrSteps(LESS_THAN)
  }

  def equal(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a == b) {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 1)
    } else {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 0)
    }
    state.sp += nbrSteps(EQUALS)
  }

  def setRelativeBase(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), state.relativeBase)
    state.relativeBase += a
    state.sp += nbrSteps(RELATIVE_BASE)
  }


  def runProgram(state: ComputerState): ComputerState = {
    val program = state.instructions
    var opcode: Int = -1
    var suspended = false
    while (opcode != EXIT && !suspended) {
      opcode = getOpcode(program(state.sp))
      val paramModes: List[BigInt] = getParamModes(opcode, program(state.sp))
      opcode match {
        case ADD => add(state, paramModes, state.relativeBase)
        case MUL => mul(state, paramModes, state.relativeBase)
        case INPUT => {
          if (state.inputs.isEmpty) {
            suspended = true
          } else {
            input(state, paramModes, state.relativeBase)
          }
        }
        case OUTPUT => output(state, paramModes, state.relativeBase)
        case JNZ => jnz(state, paramModes, state.relativeBase)
        case JEZ => jez(state, paramModes, state.relativeBase)
        case LESS_THAN => lessThan(state, paramModes, state.relativeBase)
        case EQUALS => equal(state, paramModes, state.relativeBase)
        case RELATIVE_BASE => setRelativeBase(state, paramModes, state.relativeBase)
        case EXIT => state.halted = true
        case _ => {
          println("Error: Unsupported opcode: " + opcode)
          System.exit(1)
        }
      }
    }
    state
  }

  def getVal(program: mutable.Map[BigInt, BigInt], i: BigInt, mode: BigInt, relativeBase: BigInt): BigInt = {
    var address = BigInt(-1)
    if (mode == 0) {
      address = program(i)
    } else if (mode == 2) {
      address = program(i) + relativeBase
    } else {
      address = i
    }
    program(address)
  }

  def setVal(program: mutable.Map[BigInt, BigInt], i: BigInt, mode: BigInt, relativeBase: BigInt, value: BigInt): Unit = {
    var address = BigInt(-1)
    if (mode == 0) {
      address = program(i)
    } else if (mode == 2) {
      address = program(i) + relativeBase
    } else if (mode == 1) {
      throw new Exception("invalid mode")
    }
    program(address) = value
  }

  def getParamModes(opcode: BigInt, instruction: BigInt): List[BigInt] = {
    var digits = instruction / 100 // remove opcode
    var paramsLeft: Int = nbrSteps(opcode).toInt - 1
    var modes = List[BigInt]()
    while (digits > 0) {
      modes = modes :+ (digits % 10)
      digits /= 10
      paramsLeft -= 1
    }
    0.until(paramsLeft).foreach { i =>
      modes = modes :+ BigInt(0)
    }
    modes
  }

  def nbrSteps(opcode: BigInt): BigInt = {
    opcode.intValue match {
      case 3 => BigInt(2)
      case 4 => BigInt(2)
      case 5 => BigInt(3)
      case 6 => BigInt(3)
      case 9 => BigInt(2)
      case _ => BigInt(4)
    }
  }

  def getOpcode(x: BigInt): Int = {
    (x % 100).toInt
  }

}
