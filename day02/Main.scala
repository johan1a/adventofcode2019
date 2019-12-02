import scala.io.Source
import scala.collection.mutable.Queue

object Main extends App {

  val filename = "input.txt"

  println(solve(preprocess(Source.fromFile(filename).getLines)))

  def preprocess(lines: Iterator[String]): Array[Int] = {
    val q = Queue[Int]()
    lines.foreach { line =>
      val ints = line.split(",").map{ _.toInt }
      q ++= ints
    }
    val arr = q.toArray
    arr(1) = 12
    arr(2) = 2
    arr
  }

  def solve(program: Array[Int]): Int = {
    var i = 0
    while (program(i) != 99) {
      val a = program(i + 1)
      val b = program(i + 2)
      val dest = program(i + 3)
      if (program(i) == 1) {
        program(dest) = a + b
      } else if (program(i) == 2) {
        program(dest) = a * b
      } else {
        println("error!")
      }
      i += 4
    }
    program(0)
  }

}
