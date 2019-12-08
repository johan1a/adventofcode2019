import scala.io.Source

object Main extends App {

  val input = "input.txt"
  val width = 25
  val height = 6

  println(solve(input, width, height))

  def solve(filename: String, width: Int, height: Int): Int = {
    val digits: String = Source.fromFile(filename).getLines.toList.head

    var minNbrZeroes = Int.MaxValue
    var nbrOnesInMinLayer = 0
    var nbrTwosInMinLayer = 0
    var minLayer = -1
    var currLayer = 0
    var currLayerZeroes = 0
    var currLayerOnes = 0
    var currLayerTwos = 0
    var digitsLeftInLayer = width * height

    0.until(digits.length).foreach { i =>
      if(digitsLeftInLayer == 0) {
        digitsLeftInLayer = width * height
        if(currLayerZeroes < minNbrZeroes) {
          minLayer = currLayer
          minNbrZeroes = currLayerZeroes
          nbrOnesInMinLayer = currLayerOnes
          nbrTwosInMinLayer = currLayerTwos
        }
        currLayerZeroes = 0
        currLayerOnes = 0
        currLayerTwos = 0
        digitsLeftInLayer = width * height
        currLayer += 1
      }
      if(digits.charAt(i) == '0') {
        currLayerZeroes += 1
      } else if(digits.charAt(i) == '1') {
        currLayerOnes += 1
      } else if(digits.charAt(i) == '2') {
        currLayerTwos += 1
      }
      digitsLeftInLayer -= 1
    }
    nbrOnesInMinLayer * nbrTwosInMinLayer
  }

}
