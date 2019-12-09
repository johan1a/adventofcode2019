
import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer


case class State(instructions: Map[BigInt, BigInt], var sp: BigInt, var inputs: List[BigInt], var outputs: List[BigInt], var relativeBase: BigInt = 0)

object Main extends App {

  solve("test1.txt", List())
  solve("test2.txt", List())
  solve("test3.txt", List())
  solve("input.txt", List(1))

  def solve(program: String, input: List[BigInt]): Unit = {
      var state = State(readFile(program), 0, input, List())
      state = runProgram(state)
      println("")
      println(state.outputs.mkString(","))
  }

  def readFile(filename: String): Map[BigInt, BigInt] = {
    val m = Map[BigInt, BigInt]().withDefaultValue(BigInt(0))
    Source.fromFile(filename).getLines.foreach { line =>
      line.split(",").zipWithIndex.foreach { (intAndIndex: (String, Int)) =>
        m(intAndIndex._2) = BigInt(intAndIndex._1.toLong)
      }
    }
    m
  }

  def runProgram(state: State): State = {
    var i = state.sp
    var program = state.instructions
    var opcode: BigInt = -1
    var relativeBase = state.relativeBase
    while (opcode != 99) {
      opcode = getOpcode(program(i))
      val paramModes: List[BigInt] = getParamModes(opcode, program(i))
      if (opcode.intValue == 1) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        val dest = program(i + 3)
        program(dest) = a + b
        i += nbrSteps(opcode)
      } else if (opcode.intValue == 2) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        val dest = program(i + 3)
        program(dest) = a * b
        i += nbrSteps(opcode)
      } else if (opcode.intValue == 3) {
        var a = state.inputs.head
        state.inputs = state.inputs.tail
        val dest = program(i + 1)
        program(dest) = a
        i += nbrSteps(opcode)
      } else if (opcode.intValue == 4) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        i += nbrSteps(opcode)
        println("output: " + a + " , relativeBase: " + relativeBase + ", mode: " + paramModes)
        println(i + 1)
        state.outputs = state.outputs :+ a
      } else if (opcode.intValue == 5) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        if (a != 0) {
          i = b
        } else {
          i += nbrSteps(opcode)
        }
      } else if (opcode.intValue == 6) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        if (a == 0) {
          i = b
        } else {
          i += nbrSteps(opcode)
        }
      } else if (opcode.intValue == 7) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        val c = program(i + 3)
        if (a < b) {
          program(c) = 1
        } else {
          program(c) = 0
        }
        i += nbrSteps(opcode)
      } else if (opcode == 8) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        val c = program(i + 3)
        if (a == b) {
          program(c) = 1
        } else {
          program(c) = 0
        }
        i += nbrSteps(opcode)
      } else if (opcode == 9) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        relativeBase += a
        i += nbrSteps(opcode)
      } else if (opcode == 99) {
        state.sp = i
        return state
      } else {
        println("error!")
        state
      }
    }
    state.sp = i
    return state
  }

  def getVal(program: Map[BigInt, BigInt], i: BigInt, mode: BigInt, relativeBase: BigInt): BigInt = {
    var address = BigInt(-1)
    if(mode == 0) {
      address = program(i)
    } else if(mode == 2) {
      address = program(i) + relativeBase
    } else {
      address = i
    }
    return program(address)
  }

  def getParamModes(opcode: BigInt, instruction: BigInt): List[BigInt] = {
    var digits = instruction / 100 // remove opcode
    var paramsLeft: Int = nbrSteps(opcode).toInt - 1
    var modes = List[BigInt]()
    while(digits > 0) {
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

  def getOpcode(x: BigInt): BigInt = {
    return x % 100
  }

}
