import scala.collection.mutable.Map

object Main extends App {


  def min1 = 235741
  def max1 = 706948

  println(part1(min1, max1))

  def part1(min: Int, max: Int): Int = {

    var sum = 0
    var x = min
    while(x <= max) {
      val (x2, hasRepeating) = fix(x)
      if(hasRepeating && x2 <= max){
        sum += 1
      }
      x = x2 + 1
    }
    sum
  }

  def fix(xOrig: Int): (Int, Boolean) = {
    var hasRepeating = false
    var x = xOrig
    val nbrDigits = getNbrDigits(x)
    0.until(nbrDigits - 1).reverse.foreach { i =>
      val prevDigit = getDigit(x, i + 1)
      if(getDigit(x, i) < prevDigit) {
        x = setDigit(x, i, prevDigit)
      }
      if(getDigit(x, i) == prevDigit){
        hasRepeating = true
      }
    }
    (x, hasRepeating)
  }

  def getNbrDigits(xOrig: Int): Int = {
    var sum = 0
    var x = xOrig
    while(x > 0) {
      x = x / 10
      sum += 1
    }
    sum
  }

  def getDigit(x: Int, i: Int): Int = {
    var k = x / Math.pow(10, i)
    (k % 10).toInt
  }

  def setDigit(x: Int, i: Int, newDigit: Int): Int = {
    val digitValue = (getDigit(x, i) * Math.pow(10, i)).toInt
    x - digitValue + (newDigit * Math.pow(10, i)).toInt
  }

}
