import scala.collection.mutable
import scala.io.Source

object Main extends App {

  case class ChemicalQuantity(quantity: Int, name: String)

  case class Reaction(inputs: List[ChemicalQuantity], output: ChemicalQuantity)


  val test1Result = part1("test1.txt", "ORE", "FUEL", 1)
  assert(test1Result == 31)

  val test2Result = part1("test2.txt", "ORE", "FUEL", 1)
  assert(test2Result == 165)

  val test3Result = part1("test3.txt", "ORE", "FUEL", 1)
  assert(test3Result == 13312)


  assert(part1("test4.txt", "ORE", "NVRVD", 4) == 139)
  assert(part1("test4.txt", "ORE", "JNWZP", 4) == 144)
  assert(part1("test4.txt", "ORE", "MNCFX", 6) == 145)
  assert(part1("test4.txt", "ORE", "VJHF", 6) == 176)
  assert(part1("test4.txt", "ORE", "VJHF", 1) == 176)
  assert(part1("test4.txt", "ORE", "FWMGM", 5) == 1719)
  assert(part1("test4.txt", "ORE", "RFSQX", 4) == 321)
  assert(part1("test4.txt", "ORE", "CXFTF", 8) == 139)
  assert(part1("test4.txt", "ORE", "VPVL", 8) == 839)

  println(part1("test4.txt", "ORE", "VPVL", 2))
  println(part1("test4.txt", "ORE", "FWMGM", 7))
  println(part1("test4.txt", "ORE", "CXFTF", 2))
  println(part1("test4.txt", "ORE", "MNCFX", 11))
  println(part1("test4.txt", "ORE", "STKFG", 1))


  // val test4Result = part1("test4.txt", "ORE", "FUEL", 1)
  // println(test4Result) // 180697

  // val test5Result = part1("test5.txt", "ORE", "FUEL", 1)
  // println(test5Result) // 2210736

  // val part1Result = part1("input.txt", "ORE", "FUEL")
  // println(s"Part 1: ${part1Result}")

  def part1(filename: String, source: String, target: String, wantedQty: Int = 1): Int = {
    val reactions = parseReactions(filename)
    val reaction = reactions(target)
    val needed: Map[Reaction, Int] = resolve(reactions, source, reaction, wantedQty)
    println(needed)
    needed.map { entry =>
      val react = entry._1
      val wantedOutputQty = entry._2
      val producedQty = react.output.quantity
      val requiredInputQty = react.inputs(0).quantity
      var inputSum = 0
      var outputSum = 0
      while (outputSum < wantedOutputQty) {
        outputSum += producedQty
        inputSum += requiredInputQty
      }
      inputSum
    }.sum
  }

  def resolve(reactions: mutable.Map[String, Reaction], source: String, reaction: Reaction, wantedQty: Int): Map[Reaction, Int] = {
    val producedQty = reaction.output.quantity
    var factor = 1
    if (wantedQty > producedQty) {
      factor = getFactor(wantedQty, producedQty)
    }

    if (reaction.inputs.size == 1 && reaction.inputs(0).name == source) {
      return Map(reaction -> wantedQty)
    }

    reaction.inputs.map { input =>
      resolve (reactions, source, reactions(input.name), factor * input.quantity)
    }.reduce { (a,b) => a ++ b.map { case (k, v) =>
        k -> (v + a.getOrElse(k, 0))
      }
    }
  }

  def getFactor(wantedQty: Int, producedQty: Int): Int = {
    var sum = 0
    var factor = 0
    while(sum < wantedQty) {
      sum += producedQty
      factor += 1
    }
    factor
  }

  def parseReactions(filename: String): mutable.Map[String, Reaction]
 = {
    val file = Source.fromFile(filename)
    val reactions = mutable.Map[String, Reaction]()
    file.getLines.foreach { line =>
      val splitted = line.split("=>")
      val output = makeChemicalQuantity(splitted(1))
      val inputs = splitted(0).split(",").map(makeChemicalQuantity(_)).toList
      reactions(output.name) = Reaction(inputs, output)
    }
    file.close()
    reactions
  }

  def makeChemicalQuantity(str: String): ChemicalQuantity = {
    val splitted = str.trim.split(" ")
    ChemicalQuantity(splitted(0).toInt, splitted(1))
  }
}
