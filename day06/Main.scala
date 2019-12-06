import scala.io.Source
import scala.collection.mutable.Map

object Main extends App {

  println(solve(readFile("test1.txt")))
  println(solve(readFile("input.txt")))

  def readFile(filename: String): Map[String, Set[String]] = {
    val parents = Map[String, Set[String]]()

    Source.fromFile(filename).getLines.foreach { line =>
      val splitted = line.split(')')
      val parent = splitted(0)
      val orbiter = splitted(1)
      if(!parents.contains(orbiter)) {
        parents(orbiter) = Set(parent)
      } else {
        parents(orbiter) = Set(parent) ++ parents(orbiter)
      }
    }
    parents
  }

  var cache = Map[String, Int]()

  def solve(parents: Map[String, Set[String]]): Int = {
    cache = Map[String, Int]()

    parents.keys.toList.map { node: String =>
      nbrOrbits(parents, node)
    }.sum
  }

  def nbrOrbits(allParents: Map[String, Set[String]], node: String): Int = {
    if(cache.contains(node) ) {
      return cache(node)
    }
    if(!allParents.contains(node)) {
      return 0
    }
    val parents = allParents(node)
    val result = parents.size + parents.map { nbrOrbits(allParents, _) }.sum
    cache(node) = result
    result
  }

}
