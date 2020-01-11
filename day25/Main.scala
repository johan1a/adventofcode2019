import scala.collection.mutable
import scala.util.Random
import java.io._
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

  val INPUT_FILE                       = "input.txt"
  var names                            = mutable.Map[(Int, Int), String]()
  var interactive                      = false
  var seen: Set[(String, Set[String])] = Set()
  var triedInventories                 = Set[Set[String]]()

  part1("input.txt")

  def part1(file: String): Unit = {
    println("Starting Part 1")
    names = mutable.Map[(Int, Int), String]()
    seen = Set()
    execute()
  }

  /*
                 *                                                                - Hallway
                                          2 Observatory  -  Engineering           - Navigation
                                          1 Holo deck    -  Sick bay              - Corridor
Hot Chocolate Fountain   Science lab      0 Hull breach  -  Gift Wrapping Center
                         Crew Quarters   -1 Passages
                                         -2 Stables
                                         -3
                                         -4
-2                      -1                0


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
              inventory: Set[String] = Set()): Unit = {

    val (output, fullOutput) = runCommands(history)

    0.until(5).foreach { i =>
      println()
    }

    if (output.contains("Security")) {
      println(fullOutput)
      // interactive = true
      readLine
      // println("Jesus take the wheel!")
    }

    var x2 = x
    var y2 = y
    if (history.nonEmpty) {
      if (!output.contains("can't do") && !output.contains("ejected back to the checkpoint")) {
        val prevCommand = history.last.command
        prevCommand match {
          case "north" => {
            y2 += 1
          }
          case "west" => {
            x2 -= 1
          }
          case "east" => {
            x2 += 1

          }
          case "south" => {
            y2 -= 1
          }
          case _ =>
        }
      }
    }

    var newPlace = place
    if (output.contains("==")) {
      val outputSplit = output.split("==")
      newPlace = outputSplit(outputSplit.size - 2).trim
      if (names.contains((x2, y2))) {
        if (false && newPlace != names((x2, y2))) {
          println("--- Full output ---")
          println(fullOutput)
          println("--- End full output ---")
          println(
            s"Mismatch in names. newPlace for ($x2, $y2) was previously: ${names(x2, y2)} but Now it seems to be: $newPlace")
          println(s"history: \n${history.mkString("\n")}")
          System.exit(1)
        }
      } else {
        names((x2, y2)) = newPlace
      }
    }

    if (!output.contains("Command")) {
      println(fullOutput)
      readLine
      return
    }

    if (!newPlace.contains("Security") && seen.contains((newPlace, inventory))) {
      println(
        s"I've been here before... Bailing! (x2, y2): ($x2, $y2) newPlace: $newPlace inventory: $inventory")
      return
    }
    val state: (String, Set[String]) = (newPlace, inventory)
    seen = seen + state

    var allCommands: Set[List[String]] = getAllCommands(newPlace, inventory, output)

    if (interactive) {
      println(output)
      println(s"interactive mode: $interactive")
      println(s"You are now at: $newPlace, (x: $x2 y: $y2) with inv: ${inventory} \n")
      println("Possible cmds: " + allCommands + "\n")
      val input = readLine
      if (input.contains("auto")) {
        interactive = false
      } else {
        allCommands = Set(List(input))
      }
    }

    allCommands.foreach { commandList =>
      var inv2 = inventory
      commandList.foreach { command =>
        if (command.contains("take")) {
          val item = command.split("take ")(1)
          inv2 = inv2 + item
        }
      }

      println(fullOutput)
      printStatus(output, newPlace, x2, y2, inventory, allCommands, commandList)

      val newHistory = history ++ commandList.map { command =>
        Record(newPlace, command, (x2, y2))
      }
      execute(newPlace, x2, y2, newHistory, inv2)
    }
  }

  def printStatus(output: String,
                  newPlace: String,
                  x2: Int,
                  y2: Int,
                  inventory: Set[String],
                  allCommands: Set[List[String]],
                  commandList: List[String]): Unit = {
    if (!interactive) {
      println(output)
      println(s"You are now at: $newPlace, (x: $x2 y: $y2) with inv: ${inventory} \n")
      // println(s"Your history is: ${history.mkString("\n")}")
      println("Possible cmds: " + allCommands + "\n")
      println(s"You chose to: " + commandList)
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

  def getAllCommands(newPlace: String,
                     inventory: Set[String],
                     output: String): Set[List[String]] = {
    val dropCommands = getDropCommands(newPlace, inventory, output)
    val takeCommands = getTakeCommands(output)
    val moveCommands = getMoveCommands(output)

    if (newPlace.contains("Security")) {
      if (output.contains("Droids on this ship are lighter than the detected value")) {
        triedInventories = triedInventories + inventory
        takeCommands.map { List(_) }
      } else if (output.contains("Droids on this ship are heavier than the detected value")) {
        triedInventories = triedInventories + inventory
        takeCommands.map { List(_) }
      } else {
        // Move to pressure plates
        moveCommands.map { List(_) }
      }
    } else {
      moveCommands.map { command =>
        takeCommands.toList :+ command
      }
    }
  }

  def getDropCommands(newPlace: String, inventory: Set[String], output: String): Set[String] = {
    if (newPlace.contains("Security")) {
      inventory.map { item =>
        "drop " + item
      }
    } else {
      return Set()
    }

  }

  def getTakeCommands(output: String): Set[String] = {
    val forbidden = Set("infinite loop", "photons")
    if (!output.contains("Items")) {
      Set()
    } else {
      val splitted0 = output.split("Items here:")(1)
      val splitted1 = splitted0.split("Command?")(0)
      val rep       = splitted1.replaceAll("- ", "")
      val items     = rep.split('\n').filter(s => !s.isBlank)
      items.filter(!forbidden.contains(_)).map { "take " + _ }.toSet
    }
  }

  def getMoveCommands(output: String): Set[String] = {
    if (!output.contains("Doors")) {
      Set()
    } else {
      val doorSplit = output.split("Doors here lead:")
      val splitted0 = doorSplit(doorSplit.size - 1)
      val splitted1 = if (splitted0.contains("Items")) {
        splitted0.split("Items?")(0)
      } else {
        splitted0.split("Commands?")(0)
      }
      val rep = splitted1.replaceAll("- ", "")
      Random.shuffle(rep.split('\n').filter(s => !s.isBlank).toList).toSet
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
