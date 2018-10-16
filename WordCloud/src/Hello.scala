
import java.io.PrintWriter
import java.io.File

import scala.math.pow
import scala.io.Source

object Hello {
  def square(x: Int) = x * x
  def timesTwo(i : Int) : Int = i*2

  def test(): Unit =
  {
    var numbers = List.range(1, 5)

    println(numbers.map(i => i*2))
    println(numbers.map(timesTwo))

    var sum = 0
    numbers.foreach(sum += _)
    println(sum)

    var bList = List.range(5, 20)
    numbers = numbers ::: bList //concat

    println(numbers)

    println(numbers.filter(i => i%2 == 0))

    // foreach dziala tak samo, ale zwraca typ Unit (taki void)
  }

  def gcdFun1(a : Int, b: Int) : Int =
  {
    var aAbs = a.abs
    var bAbs = b.abs

    if(bAbs == 0) a
    else gcdFun1(bAbs, aAbs % bAbs)
  }

  def roFun2(n : Int) : Int =
  {
    (1 to n).count(x => n % x == 0)
  }

  def sigmaFun3(n : Int) : Int =
  {
    (1 to n).filter(x => n % x == 0).sum
  }

  def sigmaFun4(n : Int, a : Int) : Double =
  {
    (1 to n).filter(x => n % x == 0).map(x => pow(x, a)).sum
  }

  def eulerFun5(n : Int) : Int =
  {
    (1 to n).count(x => gcdFun1(n, x) == 1)
  }

  def isPrimeFun6(n : Int) : Boolean =
  {
    if (n <= 1)
      false
    else
      (1 to n/2).count(x => gcdFun1(n ,x) == 1) == n/2
  }

  class Complex(val real : Double, val imag : Double) {

    def +(that: Complex) =
      new Complex(this.real + that.real, this.imag + that.imag)

    def -(that: Complex) =
      new Complex(this.real - that.real, this.imag - that.imag)

    def *(that: Complex) =
      new Complex(this.real * that.real - this.imag * that.imag, this.real * that.imag + this.imag * that.real)

    def modul() : Double =
    {
      math.sqrt(real * real + imag * imag)
    }

    override def toString = real + " + " + imag + "i"

  }

  def wykl() =
  {
    val T = "ALA ma kota. ALA ma psa. Ala ma kota i kanarka"
    val t = T.toLowerCase
    var G = t.filterNot(x=>".;".contains(x)).split("\\s+").map(x=>(x,1))
    val A = G.groupBy(x=>x._1).mapValues(x=>x.length)
    var X = A.toSeq
    println(X.sortWith((x,y)=>x._2>y._2))
  }

  def worldCloud() =
  {
    val stopWords = List("a", "can", "will", "not", "using", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "could", "did", "do", "does", "doing", "down", "during", "each", "few", "for", "from", "further", "had", "has", "have", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "it", "it's", "its", "itself", "let's", "me", "more", "most", "my", "myself", "nor", "of", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own", "same", "she", "she'd", "she'll", "she's", "should", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "we", "we'd", "we'll", "we're", "we've", "were", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "would", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves")

    var Book = Source.fromFile("C:\\ksiazki\\LinearRegressionUTF8.txt", "UTF-8").mkString //.split("\\s+").toString()
    var BookClean = Book.toLowerCase.split("\\s+").filterNot(x => stopWords.contains(x)).filterNot(x => x.length() <= 2).map(x => (x, 1))
    var BookReducted = BookClean.groupBy(x => x._1).mapValues(x => x.length)
    var BookSorted = BookReducted.toSeq.sortWith((x, y) => x._2 > y._2)

    var BookSliced = BookSorted.slice(0, 50)

    val writer = new PrintWriter(new File("C:\\ksiazki\\WordCloudRegression.txt"))
    writer.write(BookSliced.flatMap(x => List(x._1)).toString())
    writer.close()

    Source.fromFile("C:\\ksiazki\\WordCloudRegression.txt").foreach { x => print(x) }
  }

  def testAllFunctions() =
  {
    println(gcdFun1(6, 42))
    println(roFun2(5))
    println(sigmaFun3(6))
    println(sigmaFun4(5, 3))

    println(eulerFun5(100))
    println(Range(1,101).filter(x => 100 % x==0).map(x=>eulerFun5(x)).sum)

    println(isPrimeFun6(13))
    println(Range(1,1000).count(isPrimeFun6))

    var a = new Complex(2, 2)
    var b = new Complex( 1, 3)
    println(b)
    println(a + b)
    println(a - b)
    println(a * b)
    println(a.modul())
  }

  def main(args: Array[String])
  {
    //testAllFunctions()
   worldCloud()

  }
}