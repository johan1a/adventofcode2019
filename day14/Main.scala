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

  val test4Result = part1("test4.txt", "ORE", "FUEL", 1)
  assert(test4Result == 180697)

  val test5Result = part1("test5.txt", "ORE", "FUEL", 1)
  assert(test5Result == 2210736)

  val part1Result = part1("input.txt", "ORE", "FUEL")
  println(s"Part 1: ${part1Result}")

  def part1(filename: String, source: String, target: String, wantedQty: Int = 1): Int = {
    val reactions = parseReactions(filename)
    val reaction = reactions(target)
    val wanted = mutable.Map[String, Int]().withDefaultValue(0)
    wanted(target) = wantedQty
    resolve(reactions, source, target, wanted)
  }

  def resolve(reactions: mutable.Map[String, Reaction], source: String, target: String, wanted: mutable.Map[String, Int]): Int = {

    var queue = mutable.Queue[String](target)

    while(!queue.isEmpty) {
      val currTarget = queue.head
      val reaction = reactions(currTarget)
      queue = queue.tail

      while (wanted(currTarget) > 0) {
        wanted(currTarget) = wanted(currTarget) - reaction.output.quantity
        reaction.inputs.foreach { input =>
          wanted(input.name) += input.quantity
        }
      }

      reaction.inputs.foreach { input =>
        if (input.name != source) {
          queue += input.name
        }
      }
    }

    wanted(source)
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
