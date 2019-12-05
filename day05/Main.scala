import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

object Main extends App {

  val filename = "input.txt"

  val input1 = Array(1)
  val input2 = Array(5)

  solve(preprocess(Source.fromFile(filename).getLines), input1)
  solve(preprocess(Source.fromFile(filename).getLines), input2)

  def preprocess(lines: Iterator[String]): Array[Int] = {
    val q = Queue[Int]()
    lines.foreach { line =>
      val ints = line.split(",").map{ _.toInt }
      q ++= ints
    }
    val arr = q.toArray
    arr
  }

  def solve(program: Array[Int], input: Array[Int]): Int = {
    var inputIndex = 0
    var i = 0
    var opcode = -1
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
        val a = input(inputIndex)
        inputIndex += 1
        val dest = program(i + 1)
        program(dest) = a
        i += nbrSteps(opcode)
      } else if (opcode == 4) {
        val a = getVal(program, i + 1, paramModes(0))
        println(a)
        i += nbrSteps(opcode)
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
        return program(0)
      } else {
        println("error!")
        return -1
      }

    }
    program(0)
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
