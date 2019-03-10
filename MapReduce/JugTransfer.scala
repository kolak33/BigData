import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object JugTransfer {




  def generateNewMove(current: Array[Int]) : (Array[Int], String) =
  {
    val buckets = Array(8, 5, 3)

    val r = scala.util.Random

    // pour water from b1 to b2 if possible
    var b1 = 0
    var b2 = 0
    do
    {
      b1 = r.nextInt(3)
      b2 = r.nextInt(3)
    }  while (b1 == b2)

    // b1 -> b2
    var newBucket = current.clone
    if(current(b1) + current(b2) <= buckets(b2)) // all will fit in new bucket
      {
        newBucket(b2) = current(b1) + current(b2)
        newBucket(b1) = 0
      }
    else
      {
        newBucket(b2) = buckets(b2) // destination is full
        newBucket(b1) = current(b1) - (buckets(b2) - current(b2))
      }
    (newBucket, b1 + " " + b2)
  }


  def main(args: Array[String]) : Unit =
  {
    // set keeps track of all done moves
    // list keeps track of recent moves

    var track = Set[String]().empty
    track += Array(8, 0, 0).mkString(" ")
    var recentMoves = ListBuffer[String]()
    recentMoves += Array(8,0,0).mkString(" ")

    var triedCurrPossibilites = ListBuffer[String]()
    var triedPossib = ListBuffer[ListBuffer[String]]()
    triedPossib += ListBuffer.empty
    //println(track)
    //println(recentMoves.last)

    var current = Array(8, 0, 0)
    while(current.deep != Array(4, 4, 0).deep)
    {
      val x = generateNewMove(current)
      val newMove = x._1
      val newPossibility = x._2
      println(triedPossib)
      if(!triedCurrPossibilites.contains(newPossibility)) {
        triedCurrPossibilites += newPossibility
        triedPossib.update(triedPossib.size - 1, triedCurrPossibilites)

        if (!track.contains(newMove.mkString(" "))) {
          current = newMove
          track += newMove.mkString(" ")
          recentMoves += newMove.mkString(" ")
          triedPossib += ListBuffer.empty
          triedCurrPossibilites = ListBuffer.empty
        }
        else {
          if (recentMoves.size > 1 && triedCurrPossibilites.size == 6) {
            while(recentMoves.size > 1 && triedPossib.last.size == 6) {
              recentMoves = recentMoves.dropRight(1) //remove last
              current = recentMoves.last.split(" ").map(_.toInt)
              println("back")
              //triedCurrPossibilites = ListBuffer.empty
              triedPossib = triedPossib.dropRight(1)
              triedCurrPossibilites = triedPossib.last
            }
          }
        }
      }
    }

    println("track set:", track)
    println("lastMove:", recentMoves.dropRight(1).last)
  }

}
