import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

object Main extends App {

  val filename = "input.txt"

  var cache = Map[(Int, Int), Int]()

  println(solve(preprocess(Source.fromFile(filename).getLines), 12, 2))

  println(day2(0, 0))


  def day2(noun: Int, verb: Int): Int = {
    if (cache.contains((noun, verb))) { return(cache((noun,verb))) }
    val target = 19690720
    var output: Int = -1
    try {
      output = solve(preprocess(Source.fromFile(filename).getLines), noun, verb)
    } catch {
      case (e:ArrayIndexOutOfBoundsException) => return -1
    }
    if (output == target) {
        return 100 * noun + verb
    }
    val a = day2(noun + 1, verb)
    if(a != -1) {return a}
    val b = day2(noun, verb + 1)
    if(b != -1) {return b}
    cache((noun, verb)) = -1
    return -1
  }

  def preprocess(lines: Iterator[String]): Array[Int] = {
    val q = Queue[Int]()
    lines.foreach { line =>
      val ints = line.split(",").map{ _.toInt }
      q ++= ints
    }
    val arr = q.toArray
    arr
  }

  def solve(program: Array[Int], noun: Int, verb: Int): Int = {
    program(1) = noun
    program(2) = verb
    var i = 0
    while (program(i) != 99) {
      val a = program(i + 1)
      val b = program(i + 2)
      val dest = program(i + 3)
      if (program(i) == 1) {
        program(dest) = program(a) + program(b)
      } else if (program(i) == 2) {
        program(dest) = program(a) * program(b)
      } else {
        println("error!")
      }
      i += 4
    }
    program(0)
  }

}
