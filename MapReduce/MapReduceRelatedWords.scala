import scala.collection.mutable.ListBuffer
import scala.io.Source


object MapReduceRelatedWords {

    def mapper() : Map[String, List[(String, Int)]] = {
      var processedData = scala.collection.mutable.Map[String, ListBuffer[String]]().withDefaultValue(ListBuffer())

      val stopWords = List("a", "can", "will", "not", "using", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "could", "did", "do", "does", "doing", "down", "during", "each", "few", "for", "from", "further", "had", "has", "have", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "it", "it's", "its", "itself", "let's", "me", "more", "most", "my", "myself", "nor", "of", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own", "same", "she", "she'd", "she'll", "she's", "should", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "we", "we'd", "we'll", "we're", "we've", "were", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "would", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves")

      var Book = Source.fromFile("C:\\ksiazki\\LinearRegressionUTF8.txt", "UTF-8").mkString //.split("\\s+").toString()
      var BookClean = Book.toLowerCase
        .filterNot(x => stopWords.contains(x))
        .filterNot(x => "`~!@#$%^&*()-=+_[{]}\\|;:\'\",<.>?/—–".contains(x))
        .replaceAll("""[\p{Punct}]""", "")
        .split("\\s+")
        .filterNot(x => x.length() <= 1)


      def emit(keyVal: String, value: String): Unit = {
        processedData(keyVal) = ListBuffer(value) ++ processedData(keyVal)
      }

      var keyWord = BookClean(0)
      BookClean.drop(1) // remove first element
      for (value <- BookClean) {
        emit(keyWord, value)
        keyWord = value
      }

      var groupedData = scala.collection.mutable.Map[String, List[(String, Int)]]().withDefaultValue(List())

      for(abc <- processedData)
        groupedData(abc._1) = abc._2.map(x => (x, 1)).groupBy(x => x._1).mapValues(x => x.length).toList.sortWith((x, y) => x._2 > y._2).take(5)

      println(groupedData)
      groupedData.toMap
    }

    def uruchom(X : Map[String, List[(String, Int)]]) : Unit = {
      //for ((key,v) <- X) {
      var startingWord = "classification"
      print(startingWord)

      val maxWords = 100
      for(i <- Range(1, maxWords)) {
        startingWord = reducer(startingWord, X)
        if(i % 20 == 0)
          println()
      }
     // }
    }

    def reducer(key:String, M:Map[String, List[(String, Int)]]) : String = {
      def emit(key:String, value:List[(String, Int)]) : String = {
        val r = scala.util.Random
        val nextWord = value(r.nextInt(value.size))
        print(" " + nextWord._1)
        nextWord._1
      }
      emit(key, M(key))
    }

    def run() {
      val X = mapper()
      uruchom(X)
    }

}
