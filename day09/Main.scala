
import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer


case class State(instructions: Map[BigInt, BigInt], var sp: BigInt, var inputs: List[BigInt], var outputs: List[BigInt], var relativeBase: BigInt = 0)

object Main extends App {

  assert(solve("test1.txt", List()) == "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
  assert(solve("test2.txt", List()) == "1219070632396864")
  assert(solve("test3.txt", List()) == "1125899906842624")
  println("starting part1")
  solve("input.txt", List(1))

  println("starting part2")
  solve("input.txt", List(2))

  def solve(program: String, input: List[BigInt]): String = {
      var state = State(readFile(program), 0, input, List())
      state = runProgram(state)
      println("")
      val output = state.outputs.mkString(",")
      println(output)
      output
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
        val dest = setVal(program, i + 3, paramModes(2), relativeBase, a + b) //getAddr(program, i + 3, paramModes(2), relativeBase)
        i += nbrSteps(opcode)
      } else if (opcode.intValue == 2) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        setVal(program, i + 3, paramModes(2), relativeBase, a * b)
        // println("dest: " + dest)
        i += nbrSteps(opcode)
      } else if (opcode.intValue == 3) {
        var a = state.inputs.head
        state.inputs = state.inputs.tail
        setVal(program, i + 1, paramModes(0), relativeBase, a)
        i += nbrSteps(opcode)
      } else if (opcode.intValue == 4) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        i += nbrSteps(opcode)
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
        if (a < b) {
          setVal(program, i + 3, paramModes(2), relativeBase, 1)
        } else {
          setVal(program, i + 3, paramModes(2), relativeBase, 0)
        }
        i += nbrSteps(opcode)
      } else if (opcode == 8) {
        val a = getVal(program, i + 1, paramModes(0), relativeBase)
        val b = getVal(program, i + 2, paramModes(1), relativeBase)
        if (a == b) {
          setVal(program, i + 3, paramModes(2), relativeBase, 1)
        } else {
          setVal(program, i + 3, paramModes(2), relativeBase, 0)
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

  def setVal(program: Map[BigInt, BigInt], i: BigInt, mode: BigInt, relativeBase: BigInt, value: BigInt): Unit = {
    var address = BigInt(-1)
    if(mode == 0) {
      address = program(i)
    } else if(mode == 2) {
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
