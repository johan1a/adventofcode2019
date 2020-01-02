import scala.io.Source

object Main extends App {

  case class Shuffle(name: String, n: Int = 0)
  val DEAL_INTO = "deal into"
  val DEAL_WITH_INCREMENT = "deal with"
  val CUT = "cut"

  assert(shuffle("test1.txt", 10).toList == List(0, 7, 4, 1, 8, 5, 2, 9, 6, 3))
  assert(shuffle("test2.txt", 10).toList == List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6))
  assert(shuffle("test3.txt", 10).toList == List(6, 3, 0, 7, 4, 1, 8, 5, 2, 9))
  assert(shuffle("test4.txt", 10).toList == List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6))

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  assert(part2("test1.txt", "10", "1", "9") == 3)
  assert(part2("test5.txt", "10", "1", "6") == 9)
  assert(part2("test5.txt", "10", "1", "7") == 0)
  assert(part2("test6.txt", "10", "1", "7") == 9)
  assert(part2("test7.txt", "10", "1", "4") == 0)
  assert(part2("test7.txt", "10", "1", "3") == 9)
  assert(part2("test8.txt", "10", "1", "2") == 7)

  println("!!! Starting Part 2 !!!")
  val part2Result = part2("input.txt")
  println(s"Part 2: ${part2Result}")

  def part1(file: String, n: Int = 10007): Int = {
    val sorted = shuffle(file, n)
    sorted.indexOf(2019)
  }

  def part2(file: String, size: String = "119315717514047", repetitions: String = "101741582076661", originalIndex: String = "2020"): BigInt = {
    val deckSize: BigInt = BigInt(size)
    var index = BigInt(originalIndex)
    var i = BigInt("0")
    val max = BigInt(repetitions)

    val shuffles = readShuffles(file)

    while (i < max) {
      if(i % 10000 == 0) {
        println(i)
      }

      index = shuffle2(deckSize, index, shuffles)
      i += 1
    }
    index
  }

  def readShuffles(filename: String): List[Shuffle] = {
    val file = Source.fromFile(filename)

    val shuffles = file.getLines.toList.reverse.map { line =>
      if (line.contains("deal into")) {
        Shuffle(DEAL_INTO)
      } else if (line.contains("deal with")) {
        Shuffle(DEAL_WITH_INCREMENT, line.replace("deal with increment ", "").toInt)
      } else {
        Shuffle(CUT, line.replace("cut ", "").toInt)
      }
    }.toList
    file.close
    shuffles
  }

  def shuffle2(deckSize: BigInt, originalIndex: BigInt, shuffles: List[Shuffle]): BigInt = {
    var index = originalIndex

    shuffles.foreach { shuffle =>
      if (shuffle.name == DEAL_INTO) {
        index = dealIntoNewStack2(deckSize, index)
      } else if (shuffle.name == DEAL_WITH_INCREMENT) {
        index = dealWithIncrement2(deckSize, index, shuffle.n)
      } else if (shuffle.name == CUT) {
        index = cut2(deckSize, index, shuffle.n)
      }
    }
    index
  }

  def dealIntoNewStack2(deckSize: BigInt, index: BigInt): BigInt = {
    deckSize - index - 1
  }

  def dealWithIncrement2(deckSize: BigInt, index: BigInt, n: Int): BigInt = {
    var k = n - index % n
    if (k == n) {
      k = 0
    }
    (index  + deckSize * k) / n
  }

  def cut2(deckSize: BigInt, index: BigInt, n: Int): BigInt = {
    if (n > 0) {
      if (index < deckSize - n) {
        index + n
      } else {
        index - (deckSize - n)
      }
    } else {
      val positiveN = Math.abs(n)
      if (index < positiveN) {
        index + (deckSize - positiveN)
      } else {
        index - positiveN
      }
    }
  }

  def shuffle(filename: String, n: Int): Array[Int] = {
    var deck = 0.until(n).toArray

    val file = Source.fromFile(filename)
    val shuffles = file.getLines.toList
    file.close()

    shuffles.foreach { line =>
      if (line.contains("deal into")) {
        deck = dealIntoNewStack(deck)
      } else if (line.contains("deal with")) {
        deck = dealWithIncrement(deck, line.replace("deal with increment ", "").toInt)
      } else if (line.contains("cut")) {
        deck = cut(deck, line.replace("cut ", "").toInt)
      }
    }
    deck
  }

  def dealWithIncrement(deck: Array[Int], n: Int): Array[Int] = {
    val newDeck = Array.fill(deck.size)(0)
    deck.indices.foreach { i =>
      val j = i * n % deck.length
      newDeck(j) = deck(i)
    }
    newDeck
  }

  def dealIntoNewStack(deck: Array[Int]): Array[Int] = {
    deck.reverse
  }

  def cut(deck: Array[Int], n: Int): Array[Int] = {
    if (n > 0) {
      deck.drop(n) ++ deck.take(n)
    } else {
      val positiveN = Math.abs(n)
      deck.drop(deck.length - positiveN) ++ deck.take(deck.length - positiveN)
    }
  }
}
