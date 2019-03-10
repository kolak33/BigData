import scala.collection.mutable.{Set => MSet}
import scala.util.hashing.MurmurHash3

object MinHash {
  def minHash(s1 : String, s2 : String, k : Integer, h : Integer) : Double =
  {
    var s1KGram = s1.iterator.sliding(k).withPartial(false).map(x=>x.mkString).toSet
    var s2KGram = s2.iterator.sliding(k).withPartial(false).map(x=>x.mkString).toSet

    var s1MinHash = MSet.empty[Int]
    var s2MinHash = MSet.empty[Int]

    val r = scala.util.Random
    for (i <- Range(1, h + 1))
    {
      val hashKey = 13 * r.nextInt + 51
      var s1Hash = MSet.empty[Int]
      for (window <- s1KGram)
        s1Hash += MurmurHash3.stringHash(window, hashKey)
      s1MinHash += s1Hash.min

      var s2Hash = MSet.empty[Int]
      for (window <- s2KGram)
        s2Hash += MurmurHash3.stringHash(window, hashKey)
      s2MinHash += s2Hash.min
    }
    return (s1MinHash & s2MinHash).size.toDouble / s1MinHash.size.toDouble
  }
}
