import scala.collection.mutable
import scala.collection.mutable.Map
import scala.io.Source

val LEFT = "L"
val RIGHT = "R"
val UP = "U"
val DOWN = "D"

val BLACK = "."
val WHITE = "#"

case class State(instructions: mutable.Map[BigInt, BigInt],
                 var sp: BigInt,
                 var inputs: List[BigInt],
                 var outputs: List[BigInt],
                 var relativeBase: BigInt = 0,
                 var halted: Boolean = false)

case class RobotState(x: Int, y: Int, direction: String = UP)

val colorVals = Map(BLACK -> 0, WHITE -> 1)

object Main extends App {

  println("starting part1")
  solve("input.txt")

  def solve(program: String): String = {
    val hull = mutable.Map[(Int, Int), String]().withDefaultValue(BLACK)
    var state = State(readFile(program), 0, input, List())
    val robotState = new RobotState(0, 0)
    while (!state.halted) {
      val color = hull((robotState.x, robotState.y))
      state.inputs = List(colorVals(color))
      state = runProgram(state)
      if(state.outputs.nonEmpty) {

      }

    }
    println("")
    val output = state.outputs.mkString(",")
    println(output)
    output
  }

  def readFile(filename: String): mutable.Map[BigInt, BigInt] = {
    val m = Map[BigInt, BigInt]().withDefaultValue(BigInt(0))
    Source.fromFile(filename).getLines.foreach { line =>
      line.split(",").zipWithIndex.foreach { (intAndIndex: (String, Int)) =>
        m(intAndIndex._2) = BigInt(intAndIndex._1.toLong)
      }
    }
    m
  }

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

  def add(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a + b)
    state.sp += nbrSteps(ADD)
  }

  def mul(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a * b)
    state.sp += nbrSteps(MUL)
  }

  def input(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = state.inputs.head
    state.inputs = state.inputs.tail
    setVal(state.instructions, state.sp + 1, paramModes.head, relativeBase, a)
    state.sp += nbrSteps(INPUT)
  }

  def output(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    state.sp += nbrSteps(OUTPUT)
    state.outputs = state.outputs :+ a
  }

  def jnz(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a != 0) {
      state.sp = b
    } else {
      state.sp += nbrSteps(JNZ)
    }
  }

  def jez(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a == 0) {
      state.sp = b
    } else {
      state.sp += nbrSteps(JEZ)
    }
  }

  def lessThan(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a < b) {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 1)
    } else {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 0)
    }
    state.sp += nbrSteps(LESS_THAN)
  }

  def equal(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b = getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a == b) {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 1)
    } else {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 0)
    }
    state.sp += nbrSteps(EQUALS)
  }

  def setRelativeBase(state: State, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), state.relativeBase)
    state.relativeBase += a
    state.sp += nbrSteps(RELATIVE_BASE)
  }


  def runProgram(state: State): State = {
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
        case _ => println("Error: Unsupported opcode!")
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
      modes = modes :+ 0
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
