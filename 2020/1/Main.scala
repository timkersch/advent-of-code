import scala.io.Source

val filename = "input.txt"
val input : Array[Int] = Source.fromFile(filename).getLines.toArray.map{x => x.toInt}.sorted

def comprehend(arr: Array[Int], sum: Int) : Option[Int] = {
     if (arr.length < 2) return None
     val head = arr(0)
     val tail = arr(arr.length-1)
     if (head + tail > sum) {
         comprehend(arr.slice(0, arr.length-1), sum)
      } else if (head + tail < sum) {
          comprehend(arr.tail, sum)
      } else {
          Some(head * tail)
      }
}

println(comprehend(input, 2020).get)


// Part 2
def threeSums(arr: Array[Int]) : Int = { 
    val res : Option[Int] = comprehend(arr.tail, 2020 - arr.head)
    res match {
        case None => threeSums(arr.tail)
        case _ => arr.head * res.get
    }
}

println(threeSums(input))