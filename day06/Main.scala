import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.Queue

object Main extends App {

  //println(solve(readFile("test1.txt")))
  println(solve(readFile("input.txt")))
  //println(solve2(makeEdges(readFile("test2.txt"))))
  println(solve2(makeEdges(readFile("input.txt"))))

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

  def makeEdges(parents: Map[String, Set[String]]): Map[String, Set[String]] = {
    val edges = Map[String, Set[String]]()
    parents.keys.foreach { n =>
      val theparents = parents(n)
      if(!edges.contains(n)) {
          edges(n) = Set()
      }
      edges(n) = edges(n) ++ theparents
      theparents.foreach { parent =>
        if(!edges.contains(parent)) {
          edges(parent) = Set()
        }
        edges(parent) = edges(parent) ++ Set(n)
      }
    }
    edges
  }

  def solve2(edges: Map[String, Set[String]]): Int = {
    val start = "YOU"
    val goal = "SAN"
    var dists = Map[String, Int](start -> 0)
    var visited = Set[String]()
    var set: Set[String] = edges.keys.toSet
    var prev = null

    while(!set.isEmpty) {
      val prospects = set.filter { dists.contains(_) }
      val curr = set.filter { dists.contains(_) }.minBy { x => dists(x) }
      set = set - curr

      val neighbours = edges(curr)
      neighbours.foreach { n =>
        if(!dists.contains(n)) {
          dists(n) = dists(curr) + 1
        } else {
          dists(n) = Math.min(dists(n), dists(curr) + 1)
        }
      }

      set = set ++ neighbours.filter { x => set.contains(x) }
    }

    dists(goal) - 2 // minus two because we treat start and goal as nodes
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
