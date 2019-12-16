import scala.io.Source

case class Vec(var x: Int,
               var y: Int,
               var z: Int)

case class Moon(var pos: Vec, var velocity: Vec = Vec(0, 0, 0))

case class SingleMoon(var pos: Int, var velocity: Int = 0)

object Main extends App {

  val part1Answer = part1("input.txt")
  println(s"Part 1: ${part1Answer}")
  assert(part1Answer == 8044)

  assert(part2("test0.txt") == 2772)
  assert(part2("test1.txt") == BigInt(4686774924L))

  val part2Answer = part2("input.txt")
  assert(part2Answer == BigInt(362375881472136L))
  println(s"Part 2: ${part2Answer}")

  def part1(filename: String): Int = {
    val file = Source.fromFile(filename)
    val moons: Array[Moon] = file.getLines.map { line =>
      makeMoon(line)
    }.toArray
    simulate(moons, 1000)
  }

  def part2(filename: String): BigInt = {
    val file = Source.fromFile(filename)

    val moons: Array[Moon] = file.getLines.map { line =>
      makeMoon(line)
    }.toArray

    var singlesX = makeSingles(moons, 'x')
    var singlesY = makeSingles(moons, 'y')
    var singlesZ = makeSingles(moons, 'z')

    var periodX = findPeriod(singlesX)
    var periodY = findPeriod(singlesY)
    var periodZ = findPeriod(singlesZ)
    lcm(lcm(periodX, periodY), periodZ)
  }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / gcd(a, b)

  def makeSingles(moons: Array[Moon], coord: Char): Array[SingleMoon] = {
    coord match {
      case 'x' => moons.toList.map(moon => SingleMoon(moon.pos.x, moon.velocity.x)).toArray
      case 'y' => moons.toList.map(moon => SingleMoon(moon.pos.y, moon.velocity.y)).toArray
      case 'z' => moons.toList.map(moon => SingleMoon(moon.pos.z, moon.velocity.z)).toArray
    }
  }

  def findPeriod(moons: Array[SingleMoon]): Int = {

    var originalState = copyMoons(moons)

    var i = 0
    do {
      updateGravities(moons)
      updateVelocities(moons)
      i += 1
    } while (!equal(originalState, moons))
    i
  }

  def copyMoons(moons: Array[SingleMoon]): Array[SingleMoon] = {
    moons.map { moon => SingleMoon(moon.pos, moon.velocity) }.toArray
  }

  def equal(a: Array[SingleMoon], b: Array[SingleMoon]): Boolean = {
    a.indices.foreach { i =>
      if (a(i) != b(i)) {
        return false
      }
    }
    return true
  }

  def singleEnergy(moons: Array[SingleMoon]): Int = {
    moons.map { moon => Math.abs(moon.pos) * Math.abs(moon.velocity) }.sum
  }

  def simulate(moons: Array[Moon], nbrSteps: Int): Int = {

    0.until(nbrSteps).foreach { i =>
      updateGravities(moons)
      updateVelocities(moons)
    }

    calculateEnergy(moons)
  }

  def calculateEnergy(moons: Array[Moon]): Int = {
    calculateEnergies(moons).sum
  }

  def calculateEnergies(moons: Array[Moon]): Array[Int] = {
    moons.indices.map { i =>
      potentialEnergy(moons(i)) * kineticEnergy(moons(i))
    }.toArray
  }

  def potentialEnergy(moon: Moon): Int = {
    sum(moon.pos)
  }

  def kineticEnergy(moon: Moon): Int = {
    sum(moon.velocity)
  }

  def sum(vec: Vec): Int = {
    Math.abs(vec.x) + Math.abs(vec.y) + Math.abs(vec.z)
  }

  def updateGravities(moons: Array[SingleMoon]): Unit = {
    moons.indices.foreach { i =>
      val a = moons(i)
      (i + 1).until(moons.length).foreach { j =>
        val b = moons(j)
        if (a.pos < b.pos) {
          a.velocity += 1
          b.velocity -= 1
        } else if (a.pos > b.pos) {
          a.velocity -= 1
          b.velocity += 1
        }
      }
    }
  }

  def updateGravities(moons: Array[Moon]): Unit = {
    moons.indices.foreach { i =>
      val a = moons(i)
      (i + 1).until(moons.length).foreach { j =>
        val b = moons(j)
        if (a.pos.x < b.pos.x) {
          a.velocity.x += 1
          b.velocity.x -= 1
        } else if (a.pos.x > b.pos.x) {
          a.velocity.x -= 1
          b.velocity.x += 1
        }

        if (a.pos.y < b.pos.y) {
          a.velocity.y += 1
          b.velocity.y -= 1
        } else if (a.pos.y > b.pos.y) {
          a.velocity.y -= 1
          b.velocity.y += 1
        }

        if (a.pos.z < b.pos.z) {
          a.velocity.z += 1
          b.velocity.z -= 1
        } else if (a.pos.z > b.pos.z) {
          a.velocity.z -= 1
          b.velocity.z += 1
        }
      }
    }
  }

  def updateVelocities(moons: Array[SingleMoon]): Unit = {
    moons.indices.foreach { i =>
      val moon = moons(i)
      moon.pos = moon.pos + moon.velocity
    }
  }

  def updateVelocities(moons: Array[Moon]): Unit = {
    moons.indices.foreach { i =>
      val moon = moons(i)
      moon.pos = add(moon.pos, moon.velocity)
    }
  }

  def add(a: Vec, b: Vec): Vec = {
    Vec(a.x + b.x, a.y + b.y, a.z + b.z)
  }

  def makeMoon(line: String): Moon = {
    val replaced = line.replace("<", "").replace(">", "")
    val splitted = replaced.split(",")
    val x = splitted(0).replace("x=", "").trim.toInt
    val y = splitted(1).replace("y=", "").trim.toInt
    val z = splitted(2).replace("z=", "").trim.toInt
    Moon(Vec(x, y, z))
  }

}
