import scala.io.Source

case class Asteroid(x: Double, y: Double)


object Main extends App {

  assert(solve("test1.txt") == Asteroid(5, 8)) // 33
  assert(solve("test2.txt") == Asteroid(1, 2)) // 35
  assert(solve("test3.txt") == Asteroid(6, 3)) // 41
  assert(solve("test4.txt") == Asteroid(11, 13)) // 210
  println("part1:")
  solve("input.txt")

  val part2Test = solve2("test5.txt", Asteroid(8, 3), 35, 17, 5)
  assert(part2Test == 1303)
  val part2Result = solve2("input.txt", Asteroid(26.0, 36.0), 200, 42, 42)
  println("part2:")
  println(part2Result)
  assert(part2Result == 829.0)

  def solve(filename: String): Asteroid = {
    var asteroids = parseFile(filename)

    val best = asteroids.maxBy { a => countVisible(asteroids, a) }
    val count = countVisible(asteroids, best)
    println(best + ", " + count + " of " + asteroids.size)
    best
  }

  def solve2(filename: String, station: Asteroid, nbrToEliminate: Int, width: Int, height: Int): Double = {
    var asteroids = parseFile(filename)
      .filter(x => x != station)
      .map { a => (a, makeAngle(station, a)) }
      .sortWith { (a, b) =>
        if (a._2 == b._2) {
          dist(station, a._1) < dist(station, b._1)
        } else {
          a._2 < b._2
        }
      }
      .toArray
    var nbrEliminated = 0
    val startAngle = 3 * Math.PI / 2.0
    var i = 0
    var prevEliminatedAngle = Double.MinValue
    while (asteroids((i + 1) % asteroids.size)._2 <= startAngle) {
      i = (i + 1) % asteroids.size
    }
    var prevEliminated: Asteroid = null
    while (nbrEliminated < nbrToEliminate) {
      val asteroid = asteroids(i)
      if (asteroid != null && asteroid._2 != prevEliminatedAngle) {
        prevEliminatedAngle = asteroid._2
        prevEliminated = asteroid._1
        asteroids(i) = null
        nbrEliminated += 1
      }
      i = (i + 1) % asteroids.size
    }
    prevEliminated.x * 100 + prevEliminated.y
  }

  def countVisible(asteroids: List[Asteroid], src: Asteroid): Int = {
    val sorted = asteroids.filter { a => a != src }.sortWith { (a, b) => dist(src, a) < dist(src, b) }.toList
    assert(sorted.size == asteroids.size - 1)
    sorted.map { a => makeAngle(src, a) }.toSet.size
  }

  def makeAngle(a: Asteroid, b: Asteroid): Double = {
    val length = dist(a, b)
    var angle = Math.atan2(precision((b.y - a.y) / length), precision((b.x - a.x) / length))

    if (angle < 0) {
      angle = 2 * Math.PI + angle
    } else {
      angle = angle % (2 * Math.PI)
    }
    assert(0 <= angle && angle < 2 * Math.PI)
    angle
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
