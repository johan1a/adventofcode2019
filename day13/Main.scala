import scala.collection.mutable
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

  val EMPTY = 0
  val WALL = 1
  val BLOCK = 2
  val HORIZONTAL_PADDLE = 3
  val BALL = 4

  val FREE_PLAY = 2

  val NEUTRAL = 0
  val LEFT = -1
  val RIGHT = 1

  val DOWN = -1
  val UP = 1

  case class ComputerState(instructions: mutable.Map[BigInt, BigInt],
                           var sp: BigInt = 0,
                           var inputs: List[BigInt] = List(),
                           var outputs: List[BigInt] = List(),
                           var relativeBase: BigInt = 0,
                           var halted: Boolean = false)

  val part1Result = part1("input.txt")
  assert(part1Result == 361)
  println(s"Part 1: ${part1Result}")

  val part2Result = part2("input.txt")
  assert(part2Result == 17590)
  println(s"Part 2: ${part2Result}")

  def part1(program: String): Int = {
    var state = ComputerState(readFile(program))

    state = runProgram(state)

    var n = state.outputs.size
    var width = Math.sqrt(n).toInt
    val img: Array[Array[Int]] = Array.fill(width)(Array.fill(width)(0))

    var maxX = -1
    var maxY = -1
    state.outputs.grouped(3).foreach { elem =>
      val x = elem(0).toInt
      val y = elem(1).toInt
      if (x > maxX) {
        maxX = x
      }
      if (y > maxY) {
        maxY = y
      }
      val blockType = elem(2).toInt
      addTile(img, x, y, blockType)
    }

    img.map { (line: Array[Int] )=>
      line.toList.count { _ == BLOCK }
    }.sum
  }

  def part2(program: String): Int = {


    val (maxX, maxY) = getDimensions(program)
    val img: Array[Array[Int]] = Array.fill(maxY + 1)(Array.fill(maxX + 1)(0))


    var state = ComputerState(readFile(program))
    state.instructions(0) = FREE_PLAY
    state.inputs = List[BigInt]()
    state = runProgram(state)

    var input = NEUTRAL
    var prevBallX = -1
    var prevBallY = -1
    var score = 0
    while (!state.halted) {
      val (ballX, ballY) = getBallPos(img)

      val (ballDirX, ballDirY) = getBallDir(ballX, ballY, prevBallX, prevBallY)
      val (paddleX, paddleY) = getPaddlePos(img)

      val nextDir = getNextDir(ballX, ballY, ballDirX, ballDirY, paddleX, paddleY)

      state.inputs = List(nextDir)
      state.outputs = List()

      prevBallX = ballX
      prevBallY = ballY
      state = runProgram(state)
      val newScore = draw(img, state.outputs, maxX, maxY, false)
      if (newScore != -1) {
        score = newScore
      }
      Thread.sleep(20)
    }

    score
  }

  def getBallDir(ballX: Int, ballY: Int, prevBallX: Int, prevBallY: Int): (Int, Int) = {
      var ballDirX = 0
      var ballDirY = 0
      if (ballX > prevBallX) {
        ballDirX = RIGHT
      } else if (ballX < prevBallX) {
        ballDirX = LEFT
      }
      if (ballY > prevBallY) {
        ballDirY = UP
      } else if (ballY < prevBallY) {
        ballDirY = DOWN
      }
      (ballDirX, ballDirY)
  }

  def getNextDir(ballX: Int, ballY: Int, ballDirX: Int, ballDirY: Int, paddleX: Int, paddleY: Int): Int = {
    if (ballY == paddleY - 1 && ballX == paddleX) {
      if(ballDirY == UP) {
        NEUTRAL
      } else if (ballDirX == LEFT) {
        RIGHT
      } else {
        LEFT
      }
    } else if (ballDirX == LEFT) {
       if(paddleX >= ballX) {
        LEFT
      } else {
        RIGHT
      }
    } else if (ballDirX == RIGHT) {
      if(paddleX <= ballX) {
        RIGHT
      } else {
        LEFT
      }
    } else {
      NEUTRAL
    }
  }

  def getPos(img: Array[Array[Int]], blockType: Int): (Int, Int) = {
    img.indices.foreach { y =>
      img(0).indices.foreach { x =>
        if( img(y)(x) == blockType ) {
          return (x, y)
        }
      }
    }
    (-1, -1)
  }

  def getBallPos(img: Array[Array[Int]]): (Int, Int) = {
    getPos(img, BALL)
  }

  def getPaddlePos(img: Array[Array[Int]]): (Int, Int) = {
    getPos(img, HORIZONTAL_PADDLE)
  }

  def getDimensions(program: String): (Int, Int) = {
    var state = ComputerState(readFile(program))

    state = runProgram(state)

    var n = state.outputs.size
    var width = Math.sqrt(n).toInt
    val img: Array[Array[Int]] = Array.fill(width)(Array.fill(width)(0))

    var maxX = -1
    var maxY = -1
    state.outputs.grouped(3).foreach { elem =>
      val x = elem(0).toInt
      val y = elem(1).toInt
      if (x > maxX) {
        maxX = x
      }
      if (y > maxY) {
        maxY = y
      }
    }
    (maxX, maxY)
  }

  def visualize(int: Int): String =
    int match {
      case EMPTY => "."
      case WALL => "W"
      case BLOCK => "B"
      case HORIZONTAL_PADDLE => "-"
      case BALL => "O"
    }

  def draw(img: Array[Array[Int]], outputs: List[BigInt], maxX: Int, maxY: Int, shouldDraw: Boolean = false): Int = {
    var score = -1
    outputs.grouped(3).foreach { elem =>
      val x = elem(0).toInt
      val y = elem(1).toInt
      val blockType = elem(2).toInt
      addTile(img, x, y, blockType)
      if (!isTile(x, blockType) && blockType != -1) {
        score = blockType
      }
    }
    0.to(maxY).foreach { row =>
      0.to(maxX).foreach { col =>
        if(shouldDraw) {
          print(visualize(img(row)(col)))
        }
      }
      if(shouldDraw) {
        println
      }
    }
    score
  }

  def isTile(x: Int, blockType: Int): Boolean = (x >= 0 && blockType >= EMPTY && blockType <= BALL)

  def addTile(img: Array[Array[Int]], x: Int, y: Int, blockType: Int): Unit = {
    if (isTile(x, blockType)) {
      img(y)(x) = blockType
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
