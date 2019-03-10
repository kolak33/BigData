import scala.collection.mutable.ListBuffer

object Hotels {
  def hotels() : Unit = {
    var data = scala.io.Source.fromFile("TwoCollisions.csv").getLines.toList.map(_.split(",").map(_.toInt))
    var found = Set[String]()
    for (v1 <- data) {
      var visits = data.filter(_ (2) == v1(2))
      var collisions = new ListBuffer[Array[Int]]

      for (v2 <- visits) {
        var others = data.filter(x => (x(0) == v2(0) && x(1) == v2(1) && x(2) != v2(2)))
        if (others.length > 0)
          for (k <- others)
            collisions += k
      }

      for (x <- collisions)
        if (collisions.filter(_ (2) == x(2)).length >= 2)
          if (v1(2) < x(2)) {
            //print(s"U${v1(2)}\tU${x(2)}")

            for (y <- collisions.filter(_ (2) == x(2)))
              found += s"U${v1(2)}\tU${x(2)}"
              //print(s"\tD${y(0)}\tH${y(1)}")

          }
    }
    println(found)
  }
}
