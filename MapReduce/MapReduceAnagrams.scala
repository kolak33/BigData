import scala.collection.mutable.ListBuffer

object MapReduceAnagrams {

  def mapper(data : List[String]) : Map[String, ListBuffer[String]] = {
    var processedData = ListBuffer[String]()

    def emit(keyVal: String): Unit = {
      processedData += keyVal
    }

    for (word <- data) {
      emit(word)
    }

    processedData.groupBy(word => word.sorted)
  }

  def uruchom(X : Map[String, ListBuffer[String]]) : Unit = {
    for ((key,v) <- X) {
      reducer(key, v)
    }
  }

  def reducer(key:String, L:ListBuffer[String]) : Unit = {
    def emit(key:String,value:ListBuffer[String]){
      println(s"for key: ${key} = ${value}")
    }
    emit(key, L)
  }

  def run()
  {
    val X = mapper(List("to", "lista", "alist", "ot", "stali", "tak"))
    uruchom(X)
  }

}
