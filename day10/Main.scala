import scala.io.Source

case class Asteroid(x: Int, y: Int)


object Main extends App {

  println(solve("test0.txt"))
  println(solve("test1.txt")) // 5,8 33
  println(solve("test2.txt")) // 1,2 35

  println(solve("input.txt"))

  def solve(filename: String): Asteroid = {
    var asteroids = parseFile(filename)

    asteroids.maxBy { a => countVisible(asteroids, a) }
  }

  def countVisible(asteroids: List[Asteroid], src: Asteroid): Int = {
    val sorted = asteroids.filter { a => a != src }.sortWith { (a, b) => dist(src, a) < dist(src, b) }.toList
    val lines = sorted.map { a => makeLine(src, a) }.toSet
    lines.size
  }

  // Lines are represented by the k in y = kx + m
  // with a in origo
  def makeLine(a: Asteroid, b: Asteroid): Double = {
    val divisor = (b.x - a.x)
    if (divisor == 0) {
      return Double.MaxValue
    }
    (b.y - a.y) / divisor
  }

  def dist(a: Asteroid, b: Asteroid): Double = {
    Math.sqrt(Math.pow(b.y - a.y, 2) + Math.pow(b.x - a.x, 2))
  }

  def parseFile(filename: String): List[Asteroid] = {
    var y = 0
    var asteroids = List[Asteroid]()
    Source.fromFile(filename).getLines.foreach { line =>
      line.zipWithIndex.foreach { entry =>
        if (entry._1 == '#') {
          val x = entry._2.toInt
          asteroids = (new Asteroid(x, y)) +: asteroids
        }
      }
      y += 1
    }
    asteroids
  }
}
