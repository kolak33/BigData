import Jaccard.jaccard
import MinHash.minHash

import scala.io.Source

object TextSimilarity {
  def textSimilarity() : Unit = {
    val text1 = Source.fromFile("cppBook1.txt", "UTF-8").mkString
    val textClean1 = text1.toLowerCase
      .filterNot(x => "`~!@#$%^&*()-=+_[{]}\\|;:\'\",<.>?/—–".contains(x))
      .replaceAll("""[\p{Punct}]""", "")

    val text2 = Source.fromFile("cppBook2.txt", "UTF-8").mkString
    val textClean2 = text2.toLowerCase
      .filterNot(x => "`~!@#$%^&*()-=+_[{]}\\|;:\'\",<.>?/—–".contains(x))
      .replaceAll("""[\p{Punct}]""", "")
    println(s"Jaccard ${jaccard(textClean1, textClean2, 7)}")
    println(s"MinHash ${minHash(textClean1, textClean2, 7, 250)}")
  }
}
