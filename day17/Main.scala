import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import io.AnsiColor._

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

  val SCAFFOLD = '#'
  val EMPTY = '.'
  val NEW_LINE = '\n'

  val UP = '^'
  val DOWN = 'v'
  val LEFT = '<'
  val RIGHT = '>'

  val INSTRUCTION_RIGHT = 'R'
  val INSTRUCTION_LEFT = 'L'

  val NBR_GROUPS = 3
  val FUNCTION_MAX_LENGTH = 11 // 20 / 2, make room for commas

  type Map = mutable.Map[Pos, Char]

  case class ComputerState(instructions: mutable.Map[BigInt, BigInt],
                           var sp: BigInt = 0,
                           var inputs: List[BigInt] = List(),
                           var outputs: List[BigInt] = List(),
                           var relativeBase: BigInt = 0,
                           var halted: Boolean = false)

  case class Pos(x: Int, y: Int)

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  val part2Result = part2("input.txt")
  println(s"Part 2: ${part2Result}")
  assert(part2Result == 1415975)

  def part1(file: String): Int = {
    var state = ComputerState(readFile(file))
    state = runProgram(state)
    val (map, maxX, maxY) = parse(state.outputs.map( _.toInt))
    parameterSum(map, maxX, maxY)
  }

  def part2(file: String): Int = {
    var state = ComputerState(readFile(file))
    state = runProgram(state)
    val inputs = analyze(state.outputs.map( _.toInt))

    state = ComputerState(readFile(file))
    state.instructions(0) = 2
    state.outputs = List()
    state.inputs = inputs.map( _.toInt )
    state = runProgram(state)

    val (map, maxX, maxY) = parse(state.outputs.map( _.toInt))
    draw(map, maxX, maxY)

    state.outputs.find ( (n: BigInt) => n > 127 || n < 0).get.toInt
  }

  def analyze(input: List[Int]): List[Char] = {
    val (map, maxX, maxY) = parse(input)
    draw(map, maxX, maxY)
    val path = findPath(map, maxX, maxY)
    val inputs = split(path)
    inputs ++ List('n', '\n')
  }

  def split(path: Array[Char]): List[Char] = {
    var groupsLeft = 3
    val end = path.size
    0.until(path.size).foreach { a0 =>
      a0.until(Math.min(end, a0 + FUNCTION_MAX_LENGTH)).foreach { a1 =>
        (a1 + 1).until(path.size).foreach { b0 =>
          (b0).until(Math.min(end, b0 + FUNCTION_MAX_LENGTH)).foreach { b1 =>
            (b1 + 1).until(path.size).foreach { c0 =>
              (c0).until(Math.min(end, c0 + FUNCTION_MAX_LENGTH)).foreach { c1 =>
                val groups = List((a0, a1), (b0, b1), (c0, c1))
                val mainRoutine = place(path, groups)
                if (mainRoutine.nonEmpty) {
                  val mainWithCommas = (mainRoutine.mkString(",") + "\n").toList
                  val a = makeRoutine(path, a0, a1)
                  val b = makeRoutine(path, b0, b1)
                  val c = makeRoutine(path, c0, c1)
                  return (mainWithCommas ++ a) ++ (b ++ c)
                }
              }
            }
          }
        }
      }
    }
    throw new RuntimeException("Did not find grouping")
  }

  def makeRoutine(path: Array[Char], start: Int, end: Int): List[Char] = {
     val length = end - start + 1
     val result = mutable.Queue[Char]()
     start.to(end).foreach { i =>
       val x = path(i)
       if ( x == LEFT ) {
         result += INSTRUCTION_LEFT
       } else if (x == RIGHT) {
         result += INSTRUCTION_RIGHT
       } else {
         result += x
       }
       if (i < end) {
         if (Set(LEFT, RIGHT).contains(path(i)) || Set(LEFT, RIGHT).contains(path(i + 1))) {
          result += ','
         }
       } else {
         result += '\n'
       }
     }
     result.toList
  }

  def place(path: Array[Char], groups: List[(Int, Int)]): List[Char] = {
    var i = 0
    val chars = Map(groups(0) -> 'A', groups(1) -> 'B', groups(2) -> 'C')
    val main = mutable.Queue[Char]()
    while (i < path.size && main.size < FUNCTION_MAX_LENGTH) {
      val found = groups.find { group =>
        val start = group._1
        val end = group._2
        val groupLength = end - start + 1
        val groupWithContent = path.drop(start).take(groupLength)
        val prefix = path.drop(i).take(groupLength)
        prefix sameElements groupWithContent
      }
      if (found.isEmpty) {
        return List()
      }
      found.map { group =>
        i += (group._2 - group._1 + 1)
        main += chars(group)
      }
    }
    main.toList
    if (i == path.size) {
      main.toList
    } else {
      List()
    }
  }

  def findPath(map: Map, maxX: Int, maxY: Int): Array[Char] = {
    val path = mutable.Queue[Char]()
    var (robotPos, robotDir) = findRobotPosAndDir(map, maxX, maxY)
    val nbrScaffoldTiles = getNbrScaffoldTiles(map)
    val visited = mutable.Set[Pos](robotPos)

    var nextDir = ' '
    while (visited.size < nbrScaffoldTiles) {
      nextDir = getNextDir(map, visited, robotPos)
      path ++= turnMoves(robotDir, nextDir)
      robotDir = nextDir
      map(robotPos) = SCAFFOLD
      robotPos = addForwardMoves(map, visited, path, robotPos, robotDir)
      map(robotPos) = robotDir
    }
    path.toArray
  }

  def getNbrScaffoldTiles(map: Map): Int = {
    map.map { x =>
      x match {
        case (_, SCAFFOLD) => 1
        case _ => 0
      }
    }.sum
  }

  def addForwardMoves(map: Map, visited: mutable.Set[Pos], path: mutable.Queue[Char], robotPos: Pos, robotDir: Char): Pos = {
    var nbrMoves = 0
    var pos = robotPos
    var nextPos = getNextPos(pos, robotDir)
    while (map.contains(nextPos) && map(nextPos) == SCAFFOLD) {
      nbrMoves += 1
      visited += nextPos
      pos = nextPos
      nextPos = getNextPos(pos, robotDir)
    }
    path ++= nbrMoves.toString.map ( _.toChar ).toList
    pos
  }

  def getNextPos(pos: Pos, dir: Char): Pos = {
    dir match {
      case UP => Pos(pos.x, pos.y - 1)
      case DOWN => Pos(pos.x, pos.y + 1)
      case LEFT => Pos(pos.x - 1, pos.y)
      case RIGHT => Pos(pos.x + 1, pos.y)
    }
  }

  def turnMoves(robotDir: Char, nextDir: Char): List[Char] = {
    if (robotDir == nextDir) {
      return List()
    }
    (robotDir, nextDir) match {
      case (LEFT, UP) => List(RIGHT)
      case (LEFT, DOWN) => List(LEFT)
      case (LEFT, RIGHT) => List(RIGHT, RIGHT)

      case (RIGHT, UP) => List(LEFT)
      case (RIGHT, DOWN) => List(RIGHT)
      case (RIGHT, LEFT) => List(RIGHT, RIGHT)

      case (UP, LEFT) => List(LEFT)
      case (UP, DOWN) => List(RIGHT, RIGHT)
      case (UP, RIGHT) => List(RIGHT)

      case (DOWN, UP) => List(RIGHT, RIGHT)
      case (DOWN, LEFT) => List(RIGHT)
      case (DOWN, RIGHT) => List(LEFT)

      case _ => throw new RuntimeException("")
    }
  }

  def getNextDir(map: Map, visited: mutable.Set[Pos], pos: Pos): Char = {
    var nextPos = neighbours(pos)
      .filter { p => map.contains(p) && map(p) == SCAFFOLD && !visited.contains(p) }
      .toList
      .head
    if (nextPos == northOf(pos)){
      UP
    } else if (nextPos == southOf(pos)) {
      DOWN
    } else if (nextPos == westOf(pos)) {
      LEFT
    } else {
      RIGHT
    }
  }

  def neighbours(pos: Pos): Set[Pos] = {
    Set(northOf(pos), southOf(pos), westOf(pos), eastOf(pos))
  }

  def findRobotPosAndDir(map: Map, maxX: Int, maxY: Int): (Pos, Char) = {
    0.to(maxY).foreach { y =>
      0.to(maxX).foreach { x =>
        val pos = Pos(x, y)
        if (Set(UP, DOWN, LEFT, RIGHT).contains(map(pos))) {
          return (pos, map(pos))
        }
      }
    }
    throw new RuntimeException("")
  }

  def parameterSum(map: Map, maxX: Int, maxY: Int): Int = {
    0.until(maxY).map { y =>
      0.until(maxX).map { x =>
        val pos = Pos(x, y)
        map(pos) match {
          case SCAFFOLD => {
            if(neighbours(pos)
              .filter { (p: Pos) => map.contains(p) && map(p) == SCAFFOLD }
              .size == 4) {
              x * y
            } else {
              0
            }
          }
          case _ => 0
        }
      }.sum
    }.sum
  }

  def draw(map: Map, maxX: Int, maxY: Int): Unit = {
    0.to(maxY).foreach { y =>
      0.to(maxX).foreach { x =>
        val pos = Pos(x, y)
        if (map.contains(pos)) {
          print(prettify(map(pos)))
        }
      }
      println()
    }
  }

  def prettify(k: Char): String = {
    k match {
      case SCAFFOLD => s"$WHITE#$RESET"
      case '.' => s" "
      case '^' => "⬆"
      case 'v' => "⬇"
      case '<' => "⬅"
      case '>' => "➡"
      case x => {
        x.toString
      }
    }
  }

  def parse(outputs: List[Int]): (Map, Int, Int) = {
    val map = mutable.Map[Pos, Char]()
    var maxX = -1
    var x = 0
    var y = 0
    outputs.foreach { k =>
      k match {
        case NEW_LINE => {
          maxX = Math.max(maxX, x)
          x = 0
          y += 1
        }
        case n => {
          map(Pos(x, y)) = n.toChar
          x += 1
        }
      }
    }
    (map, maxX - 1, y - 1)
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

  def readFileToInts(filename: String): List[Int] = {
    val q = mutable.Queue[Int]()
    val file = Source.fromFile(filename)
    file.getLines.foreach { line =>
      line.map { char =>
        q += char.toInt
      }
      q += '\n'.toInt
    }
    file.close()
    q.toList
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
