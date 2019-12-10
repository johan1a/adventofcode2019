import scala.io.Source

case class Asteroid(x: Double, y: Double)


object Main extends App {

  assert(solve("test1.txt") == Asteroid(5,8)) // 33
  assert(solve("test2.txt") == Asteroid(1,2)) // 35
  assert(solve("test3.txt") == Asteroid(6,3)) // 41
  assert(solve("test4.txt") == Asteroid(11,13)) // 210
  println("")
  solve("input.txt")

  def solve(filename: String): Asteroid = {
    var asteroids = parseFile(filename)

    val best = asteroids.maxBy { a => countVisible(asteroids, a) }
    val count = countVisible(asteroids, best)
    println(best + ", " + count + " of " + asteroids.size)
    best
  }

  def countVisible(asteroids: List[Asteroid], src: Asteroid): Int = {
    val sorted = asteroids.filter { a => a != src }.sortWith { (a, b) => dist(src, a) < dist(src, b) }.toList
    assert(sorted.size == asteroids.size - 1)
    sorted.map { a => makeVector(src, a) }.toSet.size
  }

  // Lines are represented by the k in y = kx + m
  // with a in origo
  def makeVector(a: Asteroid, b: Asteroid): (Double, Double) = {
    val length = dist(a, b)
    (precision((b.y - a.y) / length), precision((b.x - a.x) / length))
  }

  def precision(k: Double, digits: Int = 13): Double = {
    (k * Math.pow(10, digits)).floor / Math.pow(10, digits)
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
          val x = entry._2.toDouble
          asteroids = (new Asteroid(x, y)) +: asteroids
        }
      }
      y += 1
    }
    asteroids
  }
}
