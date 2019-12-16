import scala.collection.mutable
import scala.io.Source

object Main extends App {

  val LEFT = "L"
  val RIGHT = "R"
  val UP = "U"
  val DOWN = "D"

  val LEFT_TURN = 0
  val RIGHT_TURN = 1

  val BLACK = "."
  val WHITE = "#"

  val colorToInt = Map(BLACK -> 0, WHITE -> 1)
  val intToColor = Map(0 -> BLACK, 1 -> WHITE)

  val nextDir = Map(
    (UP, LEFT_TURN) -> LEFT,
    (UP, RIGHT_TURN) -> RIGHT,
    (LEFT, LEFT_TURN) -> DOWN,
    (LEFT, RIGHT_TURN) -> UP,
    (DOWN, LEFT_TURN) -> RIGHT,
    (DOWN, RIGHT_TURN) -> LEFT,
    (RIGHT, LEFT_TURN) -> UP,
    (RIGHT, RIGHT_TURN) -> DOWN)

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

  case class ComputerState(instructions: mutable.Map[BigInt, BigInt],
                           var sp: BigInt = 0,
                           var inputs: List[BigInt] = List(),
                           var outputs: List[BigInt] = List(),
                           var relativeBase: BigInt = 0,
                           var halted: Boolean = false)

  case class RobotState(var pos: (Int, Int) = (0, 0), var direction: String = UP)

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  println("Part 2:")
  part2("input.txt")

  def part1(program: String): Int = {
    val hull = mutable.Map[(Int, Int), String]().withDefaultValue(BLACK)
    solve(program, hull).size
  }

  def part2(program: String): Unit = {
    val hull = mutable.Map[(Int, Int), String]().withDefaultValue(BLACK)
    hull((0, 0)) = WHITE
    val painted = solve(program, hull)
    val maxX: Int = painted.maxBy(a => a._1)._1
    val minX: Int = painted.minBy(a => a._1)._1
    val maxY: Int = painted.maxBy(a => a._2)._2
    val minY: Int = painted.minBy(a => a._2)._2
    val img: Array[Array[String]] = Array.fill((maxY - minY) + 1)(Array.fill((maxX - minX) + 1)(BLACK))
    painted.foreach { xy =>
      img(xy._2 - minY)(xy._1 - minX) = WHITE
    }
    img.indices.reverse.foreach { row =>
      img(0).indices.foreach { col =>
        print(img(row)(col))
      }
      println("")
    }
  }

  def solve(program: String, hull: mutable.Map[(Int, Int), String]): Set[(Int, Int)] = {
    var computerState = ComputerState(readFile(program))
    val robotState = RobotState()
    var painted = Set[(Int, Int)]()
    while (!computerState.halted) {
      val color = hull(robotState.pos)
      computerState.inputs = List(colorToInt(color))
      computerState = runProgram(computerState)

      if (computerState.outputs.nonEmpty) {
        val newColor = intToColor(computerState.outputs.head.toInt)
        val newDir = nextDir((robotState.direction, computerState.outputs.last.toInt))
        computerState.outputs = List()
        if (hull(robotState.pos) != newColor) {
          hull(robotState.pos) = newColor
          painted = painted ++ Set(robotState.pos)
        }
        robotState.pos = walkForward(robotState.pos, newDir)
        robotState.direction = newDir
      }
    }
    painted
  }

  def walkForward(xy: (Int, Int), direction: String): (Int, Int) = {
    val x = xy._1
    val y = xy._2
    direction match {
      case UP => (x, y + 1)
      case LEFT => (x - 1, y)
      case RIGHT => (x + 1, y)
      case DOWN => (x, y - 1)
    }
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
