
import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer


case class State(instructions: Map[Long, Long], var sp: Long, var inputs: List[Long], var outputs: List[Long], var relativeBase: Long = 0)

object Main extends App {


  solve("test1.txt", List())
//  solve("input.txt", List(0))

  def solve(program: String, input: List[Long]): Unit = {
      var state = State(readFile(program), 0, input, List())
      println(state)
      state = runProgram(state)
      println("")
      state.outputs.foreach ( println(_) )
  }

  def readFile(filename: String): Map[Long, Long] = {
    val m = Map[Long, Long]()
    Source.fromFile(filename).getLines.foreach { line =>
      line.split(",").zipWithIndex.foreach { (intAndIndex: (String, Int)) =>
        m(intAndIndex._2) = intAndIndex._1.toLong
      }
    }
    m
  }

  def runProgram(state: State): State = {
    var i = state.sp
    var program = state.instructions
    var opcode: Long = -1
    var output = Long.MinValue
    var relativeBase = state.relativeBase
    while (opcode != 99) {
      opcode = getOpcode(program(i))
      val paramModes: List[Long] = getParamModes(opcode, program(i))
      if (opcode == 1) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        val dest = program(i + 3)
        program(dest) = a + b
        i += nbrSteps(opcode)
      } else if (opcode == 2) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        val dest = program(i + 3)
        program(dest) = a * b
        i += nbrSteps(opcode)
      } else if (opcode == 3) {
        if(state.inputs.isEmpty) {
          state.sp = i
          return state
        }
        val a = state.inputs.head
        state.inputs = state.inputs.tail
        val dest = program(i + 1)
        program(dest) = a
        i += nbrSteps(opcode)
      } else if (opcode == 4) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        i += nbrSteps(opcode)
        println("outputting: " + a)
        state.outputs = a +: state.outputs
      } else if (opcode == 5) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        if (a != 0) {
          i = b
        } else {
          i += nbrSteps(opcode)
        }
      } else if (opcode == 6) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        if (a == 0) {
          i = b
        } else {
          i += nbrSteps(opcode)
        }
      } else if (opcode == 7) {
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
        relativeBase = a
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

  def getVal(program: Map[Long, Long], i: Long, mode: Long, relativeBase: Long): Long = {
    if(mode == 0) {
      return program(program(i))
    } else if(mode == 2) {
      return program(program(i) + relativeBase)
    }
    return program(i)
  }

  def getParamModes(opcode: Long, instruction: Long): List[Long] = {
    var digits = instruction / 100 // remove opcode
    var paramsLeft: Int = nbrSteps(opcode).toInt - 1
    var modes = List[Long]()
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

  def nbrSteps(opcode: Long): Long = {
    opcode match {
      case 3 => 2
      case 4 => 2
      case 5 => 3
      case 6 => 3
      case 9 => 2
      case _ => 4
    }
  }

  def getOpcode(x: Long): Long = {
    return x % 100
  }

  def getNbrDigits(xOrig: Long): Long = {
    var sum = 0
    var x = xOrig
    while(x > 0) {
      x = x / 10
      sum += 1
    }
    sum
  }

  def getDigit(x: Long, i: Long): Long = {
    var k = x / Math.pow(10, i)
    (k % 10).toLong
  }

  def setDigit(x: Long, i: Long, newDigit: Long): Long = {
    val digitValue = (getDigit(x, i) * Math.pow(10, i)).toLong
    x - digitValue + (newDigit * Math.pow(10, i)).toLong
  }

}
