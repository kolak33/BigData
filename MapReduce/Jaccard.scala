object Jaccard {
  def jaccard(s1 : String, s2 : String, k : Integer) : Double =
  {
    var s1KGram = s1.iterator.sliding(k).withPartial(false).map(x => x.mkString).toSet
    var s2KGram = s2.iterator.sliding(k).withPartial(false).map(x => x.mkString).toSet

    return (s1KGram & s2KGram).size.toDouble / ((s1KGram | s2KGram).size.toDouble)
  }
}
