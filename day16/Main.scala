import scala.io.Source
import scala.collection.mutable

object Main extends App {

  assert(part1("test1.txt", 4) == "01029498")

  val part1Result = part1("input.txt")
  assert(part1Result == "11833188")
  println("Part 1: " + part1Result)


  def part1(file: String, n: Int = 100): String = {
    var input: List[Int] = Source.fromFile(file).getLines.toList.head.toList.map ( _.asDigit )
    var i = 0
    var output = input
    while (i < n) {
      output = fft(input)
      input = output
      i += 1
    }
    val result = output.take(8).mkString
    result
  }

  def fft(input: List[Int]): List[Int] = {
    0.until(input.size).map { i =>
      val pattern = makePattern(input, i)
      val sum = input.zip(pattern).map { tup =>
        tup._1 * tup._2
      }.sum
      Math.abs(sum) % 10
    }.toList
  }

  def makePattern(input: List[Int], i: Int): List[Int] = {
    var pattern = List.fill(1 + i)(0) ++ List.fill(1 + i)(1) ++ List.fill(1 + i)(0) ++ List.fill(1 + i)(-1)
    while (pattern.size < input.size + 1) {
      pattern = pattern ++ pattern
    }
    pattern.drop(1).take(input.size)
  }

}
