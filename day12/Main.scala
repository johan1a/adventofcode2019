import scala.io.Source

case class Vec(var x: Int,
               var y: Int,
               var z: Int)

case class Moon(var pos: Vec, var velocity: Vec = Vec(0, 0, 0))

object Main extends App {

  println(part1("input.txt"))

  def part1(filename: String): Int = {
    val file = Source.fromFile(filename)
    val moons: Array[Moon] = file.getLines.map { line =>
      makeMoon(line)
    }.toArray
    simulate(moons, 1000)
  }


  def simulate(moons: Array[Moon], nbrSteps: Int): Int = {

    0.until(nbrSteps).foreach { i =>
      updateGravities(moons)
      updateVelocities(moons)
    }

    calculateEnergy(moons)
  }

  def calculateEnergy(moons: Array[Moon]): Int = {
    var sum = 0
    moons.indices.foreach { i =>
      sum += potentialEnergy(moons(i)) * kineticEnergy(moons(i))
    }
    sum
  }

  def potentialEnergy(moon: Moon): Int = {
    Math.abs(moon.pos.x) + Math.abs(moon.pos.y) + Math.abs(moon.pos.z)
  }

  def kineticEnergy(moon: Moon): Int = {
    Math.abs(moon.velocity.x) + Math.abs(moon.velocity.y) + Math.abs(moon.velocity.z)
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
    new Moon(new Vec(x, y, z))
  }

}
