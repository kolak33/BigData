
import scala.collection.mutable.ListBuffer

object MapReduceInvertGraph {

    def mapper(data : Map[Int, List[Int]]) : Map[Int, ListBuffer[Int]] = {
      var processedData = scala.collection.mutable.Map[Int, ListBuffer[Int]]().withDefaultValue(ListBuffer())

      def emit(keyVal: Int, value:Int): Unit = {
        //processedData += (keyVal -> ListBuffer(value))
        processedData(keyVal) = ListBuffer(value) ++ processedData(keyVal)
      }

      for ((vertFrom, vertList) <- data) {
        for(vertTo <- vertList)
          emit(vertTo, vertFrom)
      }

      //processedData.groupBy(word => word.sorted)
      //data.map(t => (t._1, ListBuffer.empty ++= t._2))
      processedData.toMap
    }

    def uruchom(X : Map[Int, ListBuffer[Int]]) : Unit = {
      for ((key,v) <- X) {
        reducer(key, v)
      }
    }

    def reducer(key:Int, L:ListBuffer[Int]) : Unit = {
      def emit(key:Int,value:ListBuffer[Int]){
        println(s"for key: ${key} = ${value}")
      }
      emit(key, L)
    }

    def run()
    {
      val X = mapper(Map(1 -> List(2, 3), 2 -> List(4, 5), 5 -> List(1, 4)))
      uruchom(X)
    }
}
