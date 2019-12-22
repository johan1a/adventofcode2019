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

  // assert(part2("test2.txt", "ORE", "FUEL") == 82892753)



  def part1(filename: String, source: String, target: String, wantedQty: Long = 1): Long = {
    val reactions = parseReactions(filename)
    requiredQty(reactions, source, target, wantedQty)
  }

  def part2(filename: String, source: String, target: String, sourceQty: Long = 1000000000000L): Long = {
    val reactions = parseReactions(filename)
    var wantedQty = 460664L
    var diff = wantedQty
    var required = -1L
    while (required != wantedQty && diff > 1) {
      println("trying: " + wantedQty)
      required = requiredQty(reactions, source, target, wantedQty)
      if (required > wantedQty) {
        wantedQty += diff
        diff = diff * 2
      } else if (required < wantedQty) {
        diff = diff / 2
        wantedQty -= diff
      }
    }
    wantedQty
  }

  def requiredQty(reactions: mutable.Map[String, Reaction], source: String, target: String, wantedQty: Long): Long = {

    val wanted = mutable.Map[String, Long]().withDefaultValue(0)
    wanted(target) = wantedQty

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
