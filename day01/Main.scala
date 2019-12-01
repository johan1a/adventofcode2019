import scala.io.Source

object Main extends App {

  val filename = "input.txt"

  println(totalFuel(Source.fromFile(filename).getLines))

  println(totalFuel2(Source.fromFile(filename).getLines))


  def totalFuel(lines: Iterator[String]): Int = {
    lines.map { line =>
      (line.toInt / 3) - 2
    }.sum
  }

  def totalFuel2(lines: Iterator[String]): Int = {
    lines.map { line =>
      accFuel(line.toInt)
    }.sum
  }


  def accFuel(n: Int): Int = {
    var sum = 0
    var total = (n / 3) - 2
    while (total > 0) {
      sum += total
      total = (total / 3) - 2
    }
    sum
  }

}
