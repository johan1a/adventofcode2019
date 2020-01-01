import scala.io.Source

object Main extends App {

  assert(shuffle("test1.txt", 10).toList == List(0, 7, 4, 1, 8, 5, 2, 9, 6, 3))
  assert(shuffle("test2.txt", 10).toList == List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6))
  assert(shuffle("test3.txt", 10).toList == List(6, 3, 0, 7, 4, 1, 8, 5, 2, 9))
  assert(shuffle("test4.txt", 10).toList == List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6))

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  def part1(file: String, n: Int = 10007): Int = {
    val sorted = shuffle(file, n)
    sorted.indexOf(2019)
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
