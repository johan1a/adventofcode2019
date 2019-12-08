
import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map


case class State(instructions: Array[Int], var sp: Int, var inputs: List[Int], var outputs: List[Int])

object Main extends App {

  val program1 = "input.txt"
  val program2 = "test2.txt" // 139629729

  val phases1 = List(0,1,2,3,4)
  val phases2 = List(5,6,7,8,9)

  println(solve(program1, phases1, 0))
  println(solve2(program1, phases2, 0))

  def solve2(program: String, phases: List[Int], prevSignal: Int): Int = {
    var cache = Map[(List[Int], Int), Int]()
    findHighest2(program, phases.permutations.toList)
  }

  def findHighest2(filename: String, phasePermutations: List[List[Int]]): Int = {
    phasePermutations.map { phases =>
      tryPermutation(filename, phases)
    }.max
  }

  def tryPermutation(filename: String, phases: List[Int]): Int = {

      val states = Array.fill(phases.size)(State(readFile(filename), 0, List(), List()))
      0.until(phases.size).foreach { i =>
        states(i).inputs = List(phases(i))
      }
      var prevSignal = 0

      while (true) {
        0.until(phases.size).foreach { i =>
          var state = states(i)
          state.inputs = state.inputs :+ prevSignal
          state = runProgram(state)
          if(state.outputs.isEmpty) {
            return prevSignal
          }
          prevSignal = state.outputs.head
          state.outputs = state.outputs.tail
        }
      }
      prevSignal
  }

  def solve(program: String, phases: List[Int], prevSignal: Int): Int = {
    var cache = Map[(List[Int], Int), Int]()
    findHighest(program, phases, 0, cache)
  }

  def findHighest(program: String, phases: List[Int], prevSignal: Int, cache: Map[(List[Int], Int), Int]): Int = {
    if(phases.isEmpty) {
      return prevSignal
    }
    if(cache.contains((phases, prevSignal))) {
      return cache((phases, prevSignal))
    }

    val signals = phases.map { p =>
      var state = State(readFile(program), 0, List(p, prevSignal), List())
      state = runProgram(state)
      val signal = state.outputs.head
      findHighest(program, phases.filter( _ != p), signal, cache)
    }

    val max = signals.max
    cache((phases, prevSignal)) = max
    max
  }

  def readFile(filename: String): Array[Int] = {
    val q = Queue[Int]()
    Source.fromFile(filename).getLines.foreach { line =>
      val ints = line.split(",").map{ _.toInt }
      q ++= ints
    }
    val arr = q.toArray
    arr
  }

  def runProgram(state: State): State = {
    var i = state.sp
    var program = state.instructions
    var opcode = -1
    var output = Int.MinValue
    while (opcode != 99) {
      opcode = getOpcode(program(i))
      val paramModes = getParamModes(opcode, program(i))
      if (opcode == 1) {
        val a = getVal(program, i + 1, paramModes(0))
        val b = getVal(program, i + 2, paramModes(1))
        val dest = program(i + 3)
        program(dest) = a + b
        i += nbrSteps(opcode)
      } else if (opcode == 2) {
        val a = getVal(program, i + 1, paramModes(0))
        val b = getVal(program, i + 2, paramModes(1))
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
        val a = getVal(program, i + 1, paramModes(0))
        i += nbrSteps(opcode)
        state.outputs = a +: state.outputs
      } else if (opcode == 5) {
        val a = getVal(program, i + 1, paramModes(0))
        val b = getVal(program, i + 2, paramModes(1))
        if (a != 0) {
          i = b
        } else {
          i += nbrSteps(opcode)
        }
      } else if (opcode == 6) {
        val a = getVal(program, i + 1, paramModes(0))
        val b = getVal(program, i + 2, paramModes(1))
        if (a == 0) {
          i = b
        } else {
          i += nbrSteps(opcode)
        }
      } else if (opcode == 7) {
        val a = getVal(program, i + 1, paramModes(0))
        val b = getVal(program, i + 2, paramModes(1))
        val c = program(i + 3)
        if (a < b) {
          program(c) = 1
        } else {
          program(c) = 0
        }
        i += nbrSteps(opcode)
      } else if (opcode == 8) {
        val a = getVal(program, i + 1, paramModes(0))
        val b = getVal(program, i + 2, paramModes(1))
        val c = program(i + 3)
        if (a == b) {
          program(c) = 1
        } else {
          program(c) = 0
        }
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

  def getVal(program: Array[Int], i: Int, mode: Int): Int = {
    if(mode == 0) {
      return program(program(i))
    }
    return program(i)
  }

  def getParamModes(opcode: Int, instruction: Int): List[Int] = {
    var digits = instruction / 100 // remove opcode
    var paramsLeft = nbrSteps(opcode) - 1
    var modes = List[Int]()
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

  def nbrSteps(opcode: Int): Int = {
    opcode match {
      case 3 => 2
      case 4 => 2
      case 5 => 3
      case 6 => 3
      case _ => 4
    }
  }

  def getOpcode(x: Int): Int = {
    return x % 100
  }

  def getNbrDigits(xOrig: Int): Int = {
    var sum = 0
    var x = xOrig
    while(x > 0) {
      x = x / 10
      sum += 1
    }
    sum
  }

  def getDigit(x: Int, i: Int): Int = {
    var k = x / Math.pow(10, i)
    (k % 10).toInt
  }

  def setDigit(x: Int, i: Int, newDigit: Int): Int = {
    val digitValue = (getDigit(x, i) * Math.pow(10, i)).toInt
    x - digitValue + (newDigit * Math.pow(10, i)).toInt
  }

}
