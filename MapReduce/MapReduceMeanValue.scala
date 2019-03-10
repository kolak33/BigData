
import scala.collection.mutable.ListBuffer
import scala.math.pow



object MapReduceMeanValue {

    def mapper(data : List[Int], numReducers : Int) : Map[Int, (Int, ListBuffer[Int])] = {
      var processedData = ListBuffer[Int]()

      def emit(keyVal: Int): Unit = {
        processedData += keyVal
      }

      for (num <- data) {
        emit(num)
      }

      processedData.groupBy(t => (t % numReducers)).map(t => (t._1, (t._2.size, t._2)))
    }

    def uruchom(X : Map[Int, (Int, ListBuffer[Int])] ) : Unit = {
      for ((key,v) <- X) {
        reducer(key, v)
      }
    }

    def reducer(key:Int, PL: (Int, ListBuffer[Int]) ) : Unit = {
      def emit(key:Int, min:Int, max:Int, mean:Int, numOfDistinct:Int, distinctElem:ListBuffer[Int],
               geoMean:Double, harMean:Double){
        println(s"for key: ${key}: min = ${min}, max = ${max}, mean = ${mean}, numOfDistinct = ${numOfDistinct}," +
          s"distinct = ${distinctElem}, geometricMean: ${geoMean}, harmonicMean: ${harMean}")
      }

      var distinctList = PL._2.distinct
      var geoMean = pow(PL._2.reduceLeft(_ * _), 1.0 / PL._1)
      var harMean = PL._1 / PL._2.foldLeft(0.0)(1.0 / _ + 1.0 / _)
      emit(key, PL._2.min, PL._2.max, PL._2.sum / PL._1, distinctList.size, distinctList, geoMean, harMean)
    }

    def main(args: Array[String]): Unit =
    {
      //val X = mapper(List(5, 12, 2, 5, 10, 2), 1)
      //uruchom(X)

      //MapReduceAnagrams.run()

      //MapReduceInvertGraph.run()

      //MapReduceSQLRelation.run()

      MapReduceRelatedWords.run()
    }
}
