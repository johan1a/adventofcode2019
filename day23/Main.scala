import java.util.concurrent.CompletionException

import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable.Queue
import scala.io.Source

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

  type Grid = mutable.Map[Pos, Int]

  case class ComputerState(instructions: mutable.Map[BigInt, BigInt],
                           var sp: BigInt = 0,
                           var inputs: List[BigInt] = List(),
                           var outputs: List[BigInt] = List(),
                           var relativeBase: BigInt = 0,
                           var halted: Boolean = false,
                           var nbrExecutedInstructions: Int = 0)

  case class Pos(x: Int, y: Int)

  case class Package(x: BigInt, y: BigInt)

  case class FinishedException(message: String) extends RuntimeException

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  def part1(file: String): String = {
    val computers = mutable.Map[Int, ComputerState]()
    0.until(49).foreach { i =>
      val computer = ComputerState(readFile(file))
      computer.inputs = List(i)
      computers(i) = computer
    }
    val queues = mutable.Map[BigInt, mutable.Queue[Package]]().withDefaultValue(mutable.Queue())
    try {
      while (true) {
        0.until(49).foreach { i =>
//          println(s"Executing computer: ${i}")
          val computer = computers(i)
          if (computer.inputs.isEmpty && queues(i).nonEmpty) {
            println(s"Adding package to computer #${i}")
            val pkg = queues(i).dequeue()
            computer.inputs = List(pkg.x, pkg.y)
          }
          runProgram(computer)
          if (!computer.outputs.isEmpty) {
            addPackages(computer, queues)
          }
        }
      }
    } catch {
      case (e: FinishedException) => return e.getMessage
    }
    return "Not found"
  }

  def addPackages(computerState: ComputerState, queues: mutable.Map[BigInt, mutable.Queue[Package]]) {
    while (computerState.outputs.size >= 3) {
      val outputs = computerState.outputs
      if (outputs.head == 255) {
        throw new FinishedException(outputs(1).toString)
      }
      val pkg = Package(outputs(1), outputs(2))
      val address = outputs(0)
      queues(address) += pkg
      println(s"Added package ${pkg} for address: ${address}")
      computerState.outputs = outputs.drop(3)
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

  def add(state: ComputerState,
          paramModes: List[BigInt],
          relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a + b)
    state.sp += nbrSteps(ADD)
  }

  def mul(state: ComputerState,
          paramModes: List[BigInt],
          relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a * b)
    state.sp += nbrSteps(MUL)
  }

  def input(state: ComputerState,
            paramModes: List[BigInt],
            relativeBase: BigInt): Unit = {
    val a = if (state.inputs.nonEmpty) {
      val input = state.inputs.head
      state.inputs = state.inputs.tail
      input
    } else {
      BigInt(-1)
    }
    setVal(state.instructions, state.sp + 1, paramModes.head, relativeBase, a)
    state.sp += nbrSteps(INPUT)
  }

  def output(state: ComputerState,
             paramModes: List[BigInt],
             relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    state.sp += nbrSteps(OUTPUT)
    state.outputs = state.outputs :+ a
  }

  def jnz(state: ComputerState,
          paramModes: List[BigInt],
          relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a != 0) {
      state.sp = b
    } else {
      state.sp += nbrSteps(JNZ)
    }
  }

  def jez(state: ComputerState,
          paramModes: List[BigInt],
          relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a == 0) {
      state.sp = b
    } else {
      state.sp += nbrSteps(JEZ)
    }
  }

  def lessThan(state: ComputerState,
               paramModes: List[BigInt],
               relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a < b) {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 1)
    } else {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 0)
    }
    state.sp += nbrSteps(LESS_THAN)
  }

  def equal(state: ComputerState,
            paramModes: List[BigInt],
            relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    if (a == b) {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 1)
    } else {
      setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, 0)
    }
    state.sp += nbrSteps(EQUALS)
  }

  def setRelativeBase(state: ComputerState,
                      paramModes: List[BigInt],
                      relativeBase: BigInt): Unit = {
    val a = getVal(state.instructions,
      state.sp + 1,
      paramModes(0),
      state.relativeBase)
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
          input(state, paramModes, state.relativeBase)
        }
        case OUTPUT => output(state, paramModes, state.relativeBase)
        case JNZ => jnz(state, paramModes, state.relativeBase)
        case JEZ => jez(state, paramModes, state.relativeBase)
        case LESS_THAN => lessThan(state, paramModes, state.relativeBase)
        case EQUALS => equal(state, paramModes, state.relativeBase)
        case RELATIVE_BASE =>
          setRelativeBase(state, paramModes, state.relativeBase)
        case EXIT => state.halted = true
        case _ => {
          println("Error: Unsupported opcode: " + opcode)
          System.exit(1)
        }
      }
      state.nbrExecutedInstructions += 1
      suspended = true
    }
    state
  }

  def getVal(program: mutable.Map[BigInt, BigInt],
             i: BigInt,
             mode: BigInt,
             relativeBase: BigInt): BigInt = {
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

  def setVal(program: mutable.Map[BigInt, BigInt],
             i: BigInt,
             mode: BigInt,
             relativeBase: BigInt,
             value: BigInt): Unit = {
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
      case INPUT => BigInt(2)
      case OUTPUT => BigInt(2)
      case JNZ => BigInt(3)
      case JEZ => BigInt(3)
      case RELATIVE_BASE => BigInt(2)
      case _ => BigInt(4)
    }
  }

  def getOpcode(x: BigInt): Int = {
    (x % 100).toInt
  }
}
