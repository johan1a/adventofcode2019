import scala.io.Source

object Main extends App {

  case class Shuffle(name: String, n: Int = 0)
  val DEAL_INTO = "deal into"
  val DEAL_WITH_INCREMENT = "deal with"
  val CUT = "cut"

  assert(shuffle("test1.txt", 10).toList == List(0, 7, 4, 1, 8, 5, 2, 9, 6, 3))
  assert(shuffle("test2.txt", 10).toList == List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6))
  assert(shuffle("test3.txt", 10).toList == List(6, 3, 0, 7, 4, 1, 8, 5, 2, 9))
  assert(shuffle("test4.txt", 10).toList == List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6))

  val part1Result = part1("input.txt")
  println(s"Part 1: ${part1Result}")

  assert(part22("test1.txt", "11", "1", "9") == 3)
  assert(part22("test5.txt", "11", "1", "6") == 9)
  assert(part22("test5.txt", "11", "1", "9") == 1)
  assert(part22("test6.txt", "11", "1", "7") == 6)
  assert(part22("test7.txt", "11", "1", "4") == 0)
  assert(part22("test7.txt", "11", "1", "3") == 10)
  assert(part22("test8.txt", "11", "1", "2") == 8)

  println("!!! Starting Part 2 !!!")
  val part2Result = part22("input.txt")
  println(s"Part 2: ${part2Result}")

  def part1(file: String, n: Int = 10007): Int = {
    val sorted = shuffle(file, n)
    sorted.indexOf(2019)
  }

  def part2(file: String, size: String = "119315717514047", repetitions: String = "101741582076661", originalIndex: String = "2020"): BigInt = {
    val deckSize: BigInt = BigInt(size)
    var index = BigInt(originalIndex)
    var i = BigInt("0")
    val max = BigInt(repetitions)

    val shuffles = readShuffles(file)

    while (i < max) {
      if(i % 100000 == 0) {
        println(i)
      }

      index = shuffle2(deckSize, index, shuffles)
      i += 1
    }
    index
  }

  /*
   * Y = f(X) = A * X + B
   * Z = f(Y) = A * Y + B
   *
   * Y - Z = A * (X - Y)
   * A = (Y - Z) / (X - Y)
   *
   * B = Y - A * X
   * B = Z - A * Y
   *
   * f^n(X) = A^n * X + (A^n - 1) / (A - 1) * B
   **/
  def part22(file: String, size: String = "119315717514047", repetitions: String = "101741582076661", index: String = "2020"): BigInt = {

    val deckSize: BigInt = BigInt(size)
    val n: BigInt = BigInt(repetitions)
    val shuffles = readShuffles(file)

    val x = BigInt(index)
    val y = shuffle2(deckSize, x, shuffles)
    val z = shuffle2(deckSize, y, shuffles)
    assert(y >= 0)
    assert(z >= 0)
    val dyz = y - z
    val dxy = x - y
    println(s"dyz: $dyz, dxy: $dxy")
    val a2 = mod((y - z) / (x - y), deckSize)
    val a = mod((y - z) * modInv(x - y + deckSize, deckSize), deckSize)

    val b = mod(y - a * x, deckSize)
    val b2 = mod(y - a * x, deckSize)
    println(s"a: $a b: $b b2: $b2")
    assert(b == b2)

    //(pow(A, n, D)*X + (pow(A, n, D)-1) * modinv(A-1, D) * B) % D

   val result = if (a == 1) {
    mod(x + b * n, deckSize)
   } else {
   //    mod(a.modPow(n, deckSize) * x + (a.modPow(n, deckSize) - 1) / (a - 1) * b, deckSize)
    mod(a.modPow(n, deckSize) * x + (a.modPow(n, deckSize) - 1) * modInv(a - 1, deckSize) * b, deckSize)
   }
    // val result = mod(a.modPow(n, deckSize) * x + (a.modPow(n, deckSize) - 1) / (a - 1) * b, deckSize)
    println(s"n: $n, x: $x, y: $y, z: $z, a: $a, a2: $a2, b: $b, result: $result")
    result
  }

  // From rosetta code
  def gcdExt(u: BigInt, v: BigInt): (BigInt, BigInt, BigInt) = {
    def aux(a: BigInt, b: BigInt, x: BigInt, y: BigInt, x1: BigInt, x2: BigInt, y1: BigInt, y2: BigInt): (BigInt, BigInt, BigInt) = {
      if(b == 0) (x, y, a) else {
        val (q, r) = (a / b, mod(a, b))
        aux(b, r, x2 - q * x1, y2 - q * y1, x, x1, y, y1)
      }
    }
    aux(u, v, 1, 0, 0, 1, 1, 0)
  }

  // From rosetta code
  def modInv2(a: BigInt, m: BigInt): BigInt = {
    val (i, j, g) = gcdExt(a, m)
    if (g != 1) {
      throw new RuntimeException("g != 1")
    }
    if (i < 0) i + m else i
  }

  // taken from https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/
  def modInv(aa: BigInt, mm: BigInt) : BigInt = {
        var a = aa
        var m = mm
        var m0 = m
        var y = BigInt(0)
        var x = BigInt(1)

        if (m == 1) {
            return 0;
        }

        while (a > 1) {
            // q is quotient
            var q = a / m

            var t = m

            // m is remainder now, process
            // same as Euclid's algo
            m = mod(a, m)
            a = t
            t = y

            // Update x and y
            y = x - q * y
            x = t
        }

        // Make x positive
        if (x < 0) {
            x = x + m0
        }

        return x
    }

  def mod(a: BigInt, b: BigInt): BigInt = {
    (a % b + b) % b
  }


  def readShuffles(filename: String): List[Shuffle] = {
    val file = Source.fromFile(filename)

    val shuffles = file.getLines.toList.reverse.map { line =>
      if (line.contains("deal into")) {
        Shuffle(DEAL_INTO)
      } else if (line.contains("deal with")) {
        Shuffle(DEAL_WITH_INCREMENT, line.replace("deal with increment ", "").toInt)
      } else {
        Shuffle(CUT, line.replace("cut ", "").toInt)
      }
    }.toList
    file.close
    shuffles
  }

  def shuffle2(deckSize: BigInt, originalIndex: BigInt, shuffles: List[Shuffle]): BigInt = {
    var index = originalIndex

    shuffles.foreach { shuffle =>

      if (shuffle.name == DEAL_INTO) {
        index = reverseDealIntoNewStack(deckSize, index)
      } else if (shuffle.name == DEAL_WITH_INCREMENT) {
        index = reverseDealWithIncrement(deckSize, index, shuffle.n)
      } else if (shuffle.name == CUT) {
        index = reverseCut(deckSize, index, shuffle.n)
      }
      if(index < 0) {
        println("index is negative")
        println(index)
        println(shuffle)
      }
    }
    index
  }

  def reverseDealIntoNewStack(deckSize: BigInt, index: BigInt): BigInt = {
    deckSize - index - 1
  }

  def reverseDealWithIncrement(deckSize: BigInt, index: BigInt, n: Int): BigInt = {
    val k = mod(n - mod(index, n), n)
    (index + deckSize * k) / BigInt(n)
    // mod(index * modInv(n, deckSize), deckSize)
    mod(modInv(n, deckSize) * index, deckSize)
  }

  def reverseCut(deckSize: BigInt, index: BigInt, n: Int): BigInt = {
    mod(index + n + deckSize, deckSize)
  }

  def shuffle(filename: String, n: Int): Array[Int] = {
    var deck = 0.until(n).toArray

    val file = Source.fromFile(filename)
    val shuffles = file.getLines.toList
    file.close()

    shuffles.foreach { line =>
      if (line.contains("deal into")) {
        deck = dealIntoNewStack(deck)
      } else if (line.contains("deal with")) {
        deck = dealWithIncrement(deck, line.replace("deal with increment ", "").toInt)
      } else if (line.contains("cut")) {
        deck = cut(deck, line.replace("cut ", "").toInt)
      }
    }
    deck
  }

  def dealWithIncrement(deck: Array[Int], n: Int): Array[Int] = {
    val newDeck = Array.fill(deck.size)(0)
    deck.indices.foreach { i =>
      val j = i * n % deck.length
      newDeck(j) = deck(i)
    }
    newDeck
  }

  def dealIntoNewStack(deck: Array[Int]): Array[Int] = {
    deck.reverse
  }

  def cut(deck: Array[Int], n: Int): Array[Int] = {
    if (n > 0) {
      deck.drop(n) ++ deck.take(n)
    } else {
      val positiveN = Math.abs(n)
      deck.drop(deck.length - positiveN) ++ deck.take(deck.length - positiveN)
    }
  }
}
