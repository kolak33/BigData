import scala.collection.mutable.ListBuffer

object MapReduceTwoSets {
  def mapper(data : Map[Int, List[Int]]) : Map[Int, ListBuffer[(Int, Int)]] = {
    var processedData = scala.collection.mutable.Map[Int, ListBuffer[(Int, Int)]]().withDefaultValue(ListBuffer())

    def emit(keyVal: Int, value:Int): Unit = {
      //processedData += (keyVal -> ListBuffer(value))
      if(keyVal == 1)
        processedData(keyVal) = ListBuffer((value, f(value))) ++ processedData(keyVal)
      else
        processedData(keyVal) = ListBuffer((value, g(value))) ++ processedData(keyVal)
    }

    for ((setFrom, setList) <- data) {
      for(setVal <- setList)
        emit(setFrom, setVal)
    }

    //processedData.groupBy(word => word.sorted)
    //data.map(t => (t._1, ListBuffer.empty ++= t._2))
    processedData.toMap
  }

  def uruchom(X : Map[Int, ListBuffer[(Int, Int)]]) : Unit = {
    for ((key, v) <- X) {
      reducer(key, v)
    }
  }

  def reducer(key:Int, L:ListBuffer[(Int, Int)]) : Unit = {

   // (L.reduceLeft(_._2 min _._2), L.reduceLeft(_._2 max _._2))
  }

  def f(in:Int) : Int = {
    in^2 - 5
  }

  def g(in:Int) : Int = {
    in + 2
  }

  def run()
  {
    val X = mapper(Map(1 -> List(1, 2, 3), 2 -> List(1, 2)))
    uruchom(X)
  }
}
