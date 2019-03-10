import MapReduceInvertGraph.mapper
import scala.collection.mutable.ListBuffer

object MapReduceSQLRelation {

  def mapper(data : List[(String, String, String, String)]) : Map[(String, String), ListBuffer[(Int, String)]] = {
    var processedData = scala.collection.mutable.Map[(String, String), ListBuffer[(Int, String)]]().withDefaultValue(ListBuffer())

    def emit(keyVal: (String, String), value:(Int, String)): Unit = {
      //processedData += (keyVal -> ListBuffer(value))
      processedData(keyVal) = ListBuffer(value) ++ processedData(keyVal)
    }

    for ((table, a, b, c) <- data) {
      if(table == "R")
        emit((b, c), (1, a))
      else
        emit((a, b), (2, c))
    }

    processedData.toMap
  }

  def uruchom(X : Map[(String, String), ListBuffer[(Int, String)]]) : Unit = {
    for ((key,v) <- X) {
      reducer(key, v)
    }
  }

  def reducer(key:(String, String), L:ListBuffer[(Int, String)]) : Unit = {
    def emit(key:String, value:String){
      println(s"joined relations: ${key}, ${value}")
    }

    for(from <- L; to <- L)
      if(from._1 == 1 && to._1 == 2)
        emit(from._2, to._2)
  }

  def run()
  {
    val X = mapper(List(("R", "wiek", "imie", "nazwisko"), ("S", "imie", "nazwisko", "pesel"),
      ("R", "blabla", "imie", "nazwisko"), ("S", "Nieimie", "nazwisko", "alala")))
    uruchom(X)
  }

}
