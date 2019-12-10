import scala.io.Source

case class Asteroid(x: Double, y: Double)


object Main extends App {

  println(makeAngle((0,-1)))

  assert(solve("test1.txt") == Asteroid(5,8)) // 33
  assert(solve("test2.txt") == Asteroid(1,2)) // 35
  assert(solve("test3.txt") == Asteroid(6,3)) // 41
  assert(solve("test4.txt") == Asteroid(11,13)) // 210
  println("")
  solve("input.txt")

  println("")
  println(solve2("test5.txt", Asteroid(8, 3), 35, 17, 5))
  println(solve2("input.txt", Asteroid(26.0,36.0), 200, 42, 42))

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
      .map { a => (a, makeAngle(makeVector(station, a)))}
      .sortWith { (a, b) =>
        if (a._2 == b._2) {
          dist(station, a._1) > dist(station, b._1)
        } else {
          a._2 < b._2
        }
      }
      .toArray
    var nbrEliminated = 0
    val startAngle = Math.PI
    var i = 0
    var prevEliminatedAngle = Double.MinValue
    asteroids.foreach(a => println(a + " " + dist(station, a._1)))
    while (asteroids((i + 1) % asteroids.size)._2 <= startAngle) {
      i = (i + 1) % asteroids.size
    }
    println("starting i: " + i)
    //var eliminated = Queue[(Asteroid, Double)]()
    var lastEliminated: Asteroid = null
    while (nbrEliminated < nbrToEliminate) {
      val asteroid = asteroids(i)
      if (asteroid != null && asteroid._2 != prevEliminatedAngle) {
        draw(asteroids, width, height)
        println("eliminating: " + asteroid)
        prevEliminatedAngle = asteroid._2
        lastEliminated = asteroid._1
        asteroids(i) = null
        nbrEliminated += 1
      }
      i = i - 1
      if (i == -1) {
        i = asteroids.size - 1
      }
    }
    println(lastEliminated)
    lastEliminated.x * 100 + lastEliminated.y
  }

  def draw(asteroids: Array[(Asteroid, Double)], width: Int, height: Int): Unit = {
    val img = Array.fill(height)(Array.fill(width)('.'))
    println("")
    asteroids.foreach { a =>
      if(a != null) {
        img(a._1.y.toInt)(a._1.x.toInt) = '#'
      }
    }
    0.until(height).foreach { row =>
      0.until(width).foreach { col =>
        if(col == 8 && row == 3) {
          print("O")
        } else {
          print(img(row)(col))
        }
      }
      println("")
    }
  }

  def countVisible(asteroids: List[Asteroid], src: Asteroid): Int = {
    val sorted = asteroids.filter { a => a != src }.sortWith { (a, b) => dist(src, a) < dist(src, b) }.toList
    assert(sorted.size == asteroids.size - 1)
    sorted.map { a => makeAngle(makeVector(src, a)) }.toSet.size
  }

  // Lines are represented by the k in y = kx + m
  // with a in origo
  def makeVector(a: Asteroid, b: Asteroid): (Double, Double) = {
    val length = dist(a, b)
    // println(makeAngle(precision((b.y - a.y) / length), precision((b.x - a.x) / length)))

    (precision((b.y - a.y) / length), precision((b.x - a.x) / length))
  }

  def makeAngle(xy: (Double, Double)): Double = {
    var angle = Math.atan2(xy._2,  xy._1)

    if(angle < 0) {
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
