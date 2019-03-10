import scala.math.BigInt
import Hotels.hotels
import TextSimilarity.textSimilarity

object Primes
{

  def primes() : Unit = {
    var i = BigInt(1) << 64
    val end = i + 1000
    var count = 0
    while (i <= end ) {
    if (i.isProbablePrime(100)) {
    count += 1
    println(i)
  }

    i = i + 1
  }

    println("found number of primes: ", count)
    //println("on interval [x, y), predicting (y - x + 1) / ln(x) primes: ", (end - 1000 + 1) / scala.math.log(end - 1000))
  }

  def main(args : Array[String]) : Unit =
  {
    primes()

    hotels()

    textSimilarity()
  }
}
