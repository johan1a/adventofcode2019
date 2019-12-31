import scala.collection.mutable
import scala.io.Source

object Main extends App {

  case class ChemicalQuantity(quantity: Long, name: String)

  case class Reaction(inputs: List[ChemicalQuantity], output: ChemicalQuantity)


  val test1Result = part1("test1.txt", "ORE", "FUEL", 1)
  assert(test1Result == 31)

  val test2Result = part1("test2.txt", "ORE", "FUEL", 1)
  assert(test2Result == 165)

  val test3Result = part1("test3.txt", "ORE", "FUEL", 1)
  assert(test3Result == 13312)

  val test4Result = part1("test4.txt", "ORE", "FUEL", 1)
  assert(test4Result == 180697)

  val test5Result = part1("test5.txt", "ORE", "FUEL", 1)
  assert(test5Result == 2210736)

  val part1Result = part1("input.txt", "ORE", "FUEL")
  println(s"Part 1: ${part1Result}")

  assert(part2("test3.txt", "ORE", "FUEL") == 82892753)
  assert(part2("test4.txt", "ORE", "FUEL") == 5586022)
  assert(part2("test5.txt", "ORE", "FUEL") == 460664)

  val part2Result = part2("input.txt", "ORE", "FUEL")
  println(s"Part 2: ${part2Result}")

  def part1(filename: String, source: String, target: String, wantedQty: Long = 1): Long = {
    val reactions = parseReactions(filename)
    requiredQty(reactions, source, target, wantedQty)
  }

  def part2(filename: String, source: String, target: String, sourceQty: Long = 1000000000000L): Long = {
    val reactions = parseReactions(filename)
    val requiredForOne = requiredQty(reactions, source, target, 1)
    var guess = sourceQty / requiredForOne

    var required = requiredQty(reactions, source, target, guess)

    var diff = 1L
    while (required < sourceQty && diff > 0) {
      diff = ((sourceQty - required) / requiredForOne).toLong
      guess += diff
      required = requiredQty(reactions, source, target, guess)
    }

    while (required > sourceQty) {
      guess -= 1
      required = requiredQty(reactions, source, target, guess)
    }

    guess
  }


  def requiredQty(reactions: mutable.Map[String, Reaction], source: String, target: String, wantedQty: Long): Long = {

    val wanted = mutable.Map[String, Long]().withDefaultValue(0)
    wanted(target) = wantedQty

    var queue = mutable.Queue[String](target)

    while(!queue.isEmpty) {
      val currTarget = queue.head
      val reaction = reactions(currTarget)
      queue = queue.tail

      var factor = Math.ceil(wanted(currTarget) / reaction.output.quantity.toDouble).toLong
      wanted(currTarget) -= factor * reaction.output.quantity
      reaction.inputs.foreach { input =>
        wanted(input.name) += factor * input.quantity
      }

      reaction.inputs.foreach { input =>
        if (input.name != source) {
          queue += input.name
        }
      }
    }

    wanted(source)
  }

  def getFactor(wantedQty: Long, producedQty: Long): Long = {
    var sum = 0L
    var factor = 0L
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
    ChemicalQuantity(splitted(0).toLong, splitted(1))
  }
}
