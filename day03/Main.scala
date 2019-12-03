import scala.io.Source
import scala.collection.mutable.Map

object Main extends App {

  case class Instruction(dir: String, length: Int)

  val filename = "input.txt"
  val instructionLists = preprocess(Source.fromFile(filename).getLines)

  solve(instructionLists(0), instructionLists(1))

  def preprocess(lines: Iterator[String]): List[List[Instruction]] = {
    lines.map { line =>
      line.split(",").map{ makeInstruction(_) }.toList
    }.toList
  }

  def makeInstruction(str: String): Instruction = {
    Instruction(str.charAt(0).toString, str.substring(1).toInt)
  }

  def solve(listA: List[Instruction], listB: List[Instruction]): Unit = {
    var visitsA = Map[(Int, Int), Int]()
    var visitsB = Map[(Int, Int), Int]()
    addVisits(listA, visitsA)
    addVisits(listB, visitsB)
    var minDist = Int.MaxValue
    var minSteps = Int.MaxValue
    visitsA.foreach {
      { case ((x,y), k) =>
        val coord = (x,y)
        if(coord != (0,0) && visitsB.contains(coord)) {
          val dist = Math.abs(coord._1) + Math.abs(coord._2)
          if (dist < minDist) {
            minDist = dist
          }
          val steps = visitsA(coord) + visitsB(coord)
          if (steps < minSteps) {
            minSteps = steps
          }
        }
      }
    }
    println(minDist)
    println(minSteps)
  }


  def addVisits(instructions: List[Instruction], visits: Map[(Int, Int), Int]) {
    var x = 0
    var y = 0
    var nbrSteps = 0
    instructions.foreach { _ match {
            case Instruction(dir, length) => {
                dir match {
                  case "U" => {
                    0.until(length).foreach { i =>
                      y += 1
                      nbrSteps += 1
                      addVisit(visits, x, y, nbrSteps)
                    }
                  }
                  case "D" => {
                    0.until(length).foreach { i =>
                      y -= 1
                      nbrSteps += 1
                      addVisit(visits, x, y, nbrSteps)
                    }
                  }
                  case "L" => {
                    0.until(length).foreach { i =>
                      x -= 1
                      nbrSteps += 1
                      addVisit(visits, x, y, nbrSteps)
                    }
                  }
                  case "R" => {
                    0.until(length).foreach { i =>
                      x += 1
                      nbrSteps += 1
                      addVisit(visits, x, y, nbrSteps)
                    }
                  }
                }
              }
            }
      }
  }

  def addVisit(visits: Map[(Int,Int), Int], x: Int, y: Int, nbrSteps: Int): Unit = {
    if(!visits.contains((x, y))) {
      visits((x,y)) = nbrSteps
    }
  }


}
