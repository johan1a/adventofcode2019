import scala.io.Source

object Main extends App {

  assert(shuffle("test1.txt", 10).toList == List(0, 7, 4, 1, 8, 5, 2, 9, 6, 3))
  assert(shuffle("test2.txt", 10).toList == List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6))
  assert(shuffle("test3.txt", 10).toList == List(6, 3, 0, 7, 4, 1, 8, 5, 2, 9))
  assert(shuffle("test4.txt", 10).toList == List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6))

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  assert(part2("test1.txt", "10", "1", "9") == 3)
  println(part2("test2.txt", "10", "1", "5") )

  def part1(file: String, n: Int = 10007): Int = {
    val sorted = shuffle(file, n)
    sorted.indexOf(2019)
  }

  def part2(file: String, size: String = "119315717514047", repetitions: String = "101741582076661", originalIndex: String = "2020"): BigInt = {
    val deckSize: BigInt = BigInt(size)
    var index = BigInt(originalIndex)
    var i = BigInt("0")
    val max = BigInt(repetitions)
    while (i < max) {
      index = shuffle2(file, deckSize, index)
      i += 1
    }
    index
  }

  def shuffle2(filename: String, deckSize: BigInt, originalIndex: BigInt): BigInt = {
    var index = originalIndex
    val file = Source.fromFile(filename)
    val shuffles = file.getLines.toList
    file.close()

    shuffles.reverse.foreach { line =>
      if (line.contains("deal into")) {
        index = dealIntoNewStack2(deckSize, index)
      } else if (line.contains("deal with")) {
        index = dealWithIncrement2(deckSize, index, line.replace("deal with increment ", "").toInt)
      } else if (line.contains("cut")) {
        index = cut2(deckSize, index, line.replace("cut ", "").toInt)
      }
    }
    index
  }

  def dealIntoNewStack2(deckSize: BigInt, index: BigInt): BigInt = {
    deckSize - index
  }

  def dealWithIncrement2(deckSize: BigInt, index: BigInt, n: Int): BigInt = {
    var j = BigInt(-1)
    var i = BigInt(-1)
    while (j != index) {
      i += 1
      j = i * n % deckSize
    }
    i
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
    var j = 0
    deck.indices.foreach { i =>
      newDeck(j) = deck(i)
      j = (j + n) % deck.length
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
