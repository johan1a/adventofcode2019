import scala.collection.mutable
import scala.collection.immutable.Queue
import scala.io.Source
import scala.io.StdIn.readLine
import java.nio.file.CopyOption

object Main extends App {

  val ADD           = 1
  val MUL           = 2
  val INPUT         = 3
  val OUTPUT        = 4
  val JNZ           = 5
  val JEZ           = 6
  val LESS_THAN     = 7
  val EQUALS        = 8
  val RELATIVE_BASE = 9
  val EXIT          = 99

  val NAT_ADDRESS = 255

  case class ComputerState(instructions: mutable.Map[BigInt, BigInt],
                           var sp: BigInt = 0,
                           var inputs: List[BigInt] = List(),
                           var outputs: List[BigInt] = List(),
                           var relativeBase: BigInt = 0,
                           var halted: Boolean = false)

  case class Pos(x: BigInt, y: BigInt)

  val INPUT_FILE  = "input.txt"
  var names       = mutable.Map[(Int, Int), String]()
  var interactive = false

  part1("input.txt")

  def part1(file: String): Unit = {
    names = mutable.Map[(Int, Int), String]()
    execute()
  }

  /*
                            2 Observatory  -  Engineering
                            1 Holo deck
    Hot Chocolate Fountaint 0 Hull breach
                           -1 Passages
                           -2 Stables
                           -3
                           -4
    -1                      0


  history: Stack(Hull Breach: north
, Passages: north
, Stables: north
, Stables: take hypercube
, Passages: south
, Hull Breach: south
, Hull Breach: south

   */

  case class Record(place: String, command: String, pos: (Int, Int))

  def execute(place: String = "",
              x: Int = 0,
              y: Int = 0,
              history: Queue[Record] = Queue(),
              inventory: Set[String] = Set(),
              seen: Set[(String, Set[String])] = Set(),
              residualCommands: Set[String] = Set()): Unit = {

    if (seen.contains((place, inventory))) {
      // println(s"I've been here before... Bailing! Inventory: $inventory")
      return
    }
    val state: (String, Set[String]) = (place, inventory)
    val newSeen                      = seen + state
    println(newSeen)

    val (output, fullOutput) = runCommands(history)

    0.until(5).foreach { i =>
      println()
    }

    if (output.contains("Security Checkpoint")) {
      println("Jesus take the wheel!")
      interactive = true
    }

    var x2 = x
    var y2 = y
    if (history.nonEmpty) {
      if (output.contains("can't do") || output.contains("ejected back to the checkpoint")) {
        println(s"history: $history")
        interactive = true
      } else {
        val prevCommand = history.head.command
        prevCommand match {
          case "north" => y2 += 1
          case "west"  => x2 -= 1
          case "east"  => x2 += 1
          case "south" => y2 -= 1
          case _       =>
        }
      }
    }

    var name = place
    if (output.contains("==")) {
      val outputSplit = output.split("==")
      name = outputSplit(outputSplit.size - 2).trim
      if (names.contains((x2, y2))) {
        if (false && name != names((x2, y2))) {
          println("--- Full output ---")
          println(fullOutput)
          println("--- End full output ---")
          println(
            s"Mismatch in names. name for ($x2, $y2) was previously: ${names(x2, y2)} but Now it seems to be: $name")
          println(s"history: \n${history.mkString("\n")}")
          System.exit(1)
        }
      } else {
        names((x2, y2)) = name
      }
    }

    if (!output.contains("Command")) {
      return
    }

    val takeCommands = getTakeCommands(output)
    val moveCommands = getMoveCommands(output)

    var allCommands: Set[String] = Set()
    allCommands = (takeCommands ++ moveCommands ++ residualCommands).toSet

    if (interactive) {
      println(s"interactive is: $interactive")
      println("--- Full output ---")
      println(fullOutput)
      println("--- End full output ---")
      println(s"You are now at: $name, (x: $x2 y: $y2) with inv: ${inventory} \n")
      println("Possible cmds: " + allCommands + "\n")
      val input = readLine
      if (input.contains("auto")) {
        interactive = false
      } else {
        allCommands = Set(input)
      }
    }

    allCommands.foreach { command =>
      var newResidualCommands = Set[String]()
      var inv2                = inventory
      if (command.contains("take")) {
        val item = command.split("take ")(1)
        inv2 = inv2 + item
        newResidualCommands = (takeCommands ++ moveCommands).toSet - command
      } else {
        newResidualCommands = Set()
      }

      if (!interactive) {
        println(s"You are now at: $name, (x: $x2 y: $y2) with inv: ${inventory} \n")
        // println(s"Your history is: ${history.mkString("\n")}")
        println("Possible cmds: " + allCommands + "\n")
        println(s"You chose to: " + command)
      }

      execute(name,
              x2,
              y2,
              history :+ Record(name, command, (x2, y2)),
              inv2,
              newSeen,
              newResidualCommands)
    }
  }

  def runCommands(records: Queue[Record]): (String, String) = {
    val computer   = ComputerState(readFile(INPUT_FILE))
    var lastOutput = ""
    var fullOutput = ""
    runProgram(computer)
    fullOutput += makeOutput(computer.outputs)
    lastOutput = makeOutput(computer.outputs)

    records.foreach { record =>
      computer.outputs = List()
      computer.inputs = makeInput(record.command)
      runProgram(computer)
      fullOutput += makeOutput(computer.outputs)
      lastOutput = makeOutput(computer.outputs)
    }
    (lastOutput, fullOutput)
  }

  def makeOutput(outputs: List[BigInt]): String = {
    outputs.map(_.toChar).mkString("")
  }

  def copyState(state: ComputerState): ComputerState = {
    val instructions = mutable.Map[BigInt, BigInt]()
    state.copy().instructions.keys.foreach { k =>
      instructions(k) = state.instructions(k)
    }

    val result = ComputerState(instructions,
                               state.sp,
                               state.inputs,
                               state.outputs,
                               state.relativeBase,
                               state.halted)
    result
  }

  def getTakeCommands(output: String): List[String] = {
    val forbidden = Set("infinite loop")
    if (!output.contains("Items")) {
      List()
    } else {
      val splitted0 = output.split("Items here:")(1)
      val splitted1 = splitted0.split("Command?")(0)
      val rep       = splitted1.replaceAll("- ", "")
      val items     = rep.split('\n').filter(s => !s.isBlank)
      items.filter(!forbidden.contains(_)).map { "take " + _ }.toList
    }
  }

  def getMoveCommands(output: String): List[String] = {
    if (!output.contains("Doors")) {
      List()
    } else {
      val doorSplit = output.split("Doors here lead:")
      val splitted0 = doorSplit(doorSplit.size - 1)
      val splitted1 = if (splitted0.contains("Items")) {
        splitted0.split("Items?")(0)
      } else {
        splitted0.split("Commands?")(0)
      }
      val rep = splitted1.replaceAll("- ", "")
      rep.split('\n').filter(s => !s.isBlank).toList
    }
  }

  def makeInput(string: String): List[BigInt] = {
    var str = if (string.endsWith("\n")) string else s"$string\n"
    str.toList.map(BigInt(_))
  }

  def readFile(filename: String): mutable.Map[BigInt, BigInt] = {
    val m    = mutable.Map[BigInt, BigInt]().withDefaultValue(BigInt(0))
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
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
    setVal(state.instructions, state.sp + 3, paramModes(2), relativeBase, a + b)
    state.sp += nbrSteps(ADD)
  }

  def mul(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    val b =
      getVal(state.instructions, state.sp + 2, paramModes(1), relativeBase)
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
    val a =
      getVal(state.instructions, state.sp + 1, paramModes(0), relativeBase)
    state.sp += nbrSteps(OUTPUT)
    state.outputs = state.outputs :+ a
  }

  def jnz(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
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

  def jez(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
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

  def lessThan(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
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

  def equal(state: ComputerState, paramModes: List[BigInt], relativeBase: BigInt): Unit = {
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
    val a = getVal(state.instructions, state.sp + 1, paramModes(0), state.relativeBase)
    state.relativeBase += a
    state.sp += nbrSteps(RELATIVE_BASE)
  }

  def runProgram(state: ComputerState): ComputerState = {
    val program     = state.instructions
    var opcode: Int = -1
    var suspended   = false
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
        case OUTPUT    => output(state, paramModes, state.relativeBase)
        case JNZ       => jnz(state, paramModes, state.relativeBase)
        case JEZ       => jez(state, paramModes, state.relativeBase)
        case LESS_THAN => lessThan(state, paramModes, state.relativeBase)
        case EQUALS    => equal(state, paramModes, state.relativeBase)
        case RELATIVE_BASE =>
          setRelativeBase(state, paramModes, state.relativeBase)
        case EXIT => state.halted = true
        case _ => {
          println("Error: Unsupported opcode: " + opcode)
          System.exit(1)
        }
      }
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
    var digits          = instruction / 100 // remove opcode
    var paramsLeft: Int = nbrSteps(opcode).toInt - 1
    var modes           = List[BigInt]()
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
      case INPUT         => BigInt(2)
      case OUTPUT        => BigInt(2)
      case JNZ           => BigInt(3)
      case JEZ           => BigInt(3)
      case RELATIVE_BASE => BigInt(2)
      case _             => BigInt(4)
    }
  }

  def getOpcode(x: BigInt): Int = {
    (x % 100).toInt
  }

}
