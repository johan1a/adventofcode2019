import scala.io.Source
import scala.collection.mutable

object Main extends App {
  assert(part1("test1.txt", 4) == "01029498")

  var start = System.currentTimeMillis
  val part1Result = part1("input.txt")
  assert(part1Result == "11833188")
  println("Part 1: " + part1Result + ". Took " + (System.currentTimeMillis - start) + " millis.")

  val test2Result = part2("test2.txt")
  assert(test2Result == "84462026")

  start = System.currentTimeMillis
  val part2Result = part2("input.txt")
  println("Part 2: " + part2Result + ". Took " + (System.currentTimeMillis - start) + " millis.")

  def part2(filename: String): String = {
    var baseInput = readFileAsString(filename) * 10000
    val offset = baseInput.take(7).mkString.toInt
    var input = baseInput.map ( _.asDigit ).toList.drop(offset)

    println(s"starting FFT with input size: ${input.size}. Dropped ${offset} from ${baseInput.size}")
    val output = runFft2(input.toArray, 100)
    output.take(8).mkString
  }

  def part1(filename: String, n: Int = 100, short: Boolean = true): String = {
    println("running for: " + filename)
    var input: List[Int] = readFile(filename)
    if (short) {
      runFft(input, n).take(8).mkString
    } else {
      runFft(input, n).mkString
    }
  }

  def runFft(originalInput: List[Int], n: Int = 100): List[Int] = {
    var i = 0
    var input = originalInput
    var output = input
    while (i < n) {
      output = fft(input)
      input = output
      i += 1
    }
    output
  }

  def runFft2(originalInput: Array[Int], n: Int = 100): List[Int] = {
    var i = 0
    var input = originalInput
    var output: Array[Int] = input
    while (i < n) {
      output = fft2(input)
      input = output
      i += 1
    }
    output.toList
  }

  def fft(input: List[Int]): List[Int] = {
    var q = mutable.Queue[Int]()
    0.until(input.size).foreach { i =>
      val sum = input.drop(i).zipWithIndex.map { _ match {
          case (digit, j) => {
            patternVal(i, i + j, digit)
          }
        }
      }.sum
      q += Math.abs(sum) % 10
    }
    val res = q.toList
    res
  }

  def fft2(input: Array[Int]): Array[Int] = {
    var sum = 0
    val arr = Array.fill(input.size)(0)
    var j = input.size - 1
    while (j >= 0) {
      sum += input(j)
      arr(j) = Math.abs(sum % 10)
      j -= 1
    }
    arr
  }

  def patternVal(i: Int, j: Int, v: Int): Int = {
    val length = (i + 1) * 4
    val k = (j + 1) % length
    if (k >= length / 4 && k < length / 2) {
      v
    } else if (k >= 3 * length / 4) {
      -v
    } else {
      0
    }
  }

  def readFile(filename: String): List[Int] = {
    Source.fromFile(filename).getLines.toList.head.toList.map ( _.asDigit )
  }

  def readFileAsString(filename: String): String = {
    Source.fromFile(filename).getLines.toList.head
  }

}
