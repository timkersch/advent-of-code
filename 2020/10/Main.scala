import scala.io.Source
import scala.util.control._

val filename = "input.txt"
val input : Array[Int] = Source.fromFile(filename).getLines.toArray.map{x => x.toInt}

def distribution(skipLength: Int, arr: Array[Int]) : Int = {
    if (arr.length < 2) return 0
    if (skipLength == (arr.tail(0) - arr.head)) {
        return 1 + distribution(skipLength, arr.tail)
    } else {
        return 0 + distribution(skipLength, arr.tail)
    }
}

val arr = (input ++ Array(0, input.max + 3)).sorted
val dist1 = distribution(1, arr)
val dist3 = distribution(3, arr)
println(dist1 * dist3)

// Part 2
val mem = Array.fill[Long](arr.length)(-1)
def arrange(arr: Array[Int], index: Int) : Long = {
    // If it reaches the end, that's 1 combination
    if (arr.length == 1) {
        return 1
    } else if(mem(index) != -1) {
        return mem(index)
    } else {
        var sum : Long = 0
        val loop = new Breaks;
        loop.breakable {
            for (i <- 0 until 3) {
                if (arr.tail.length-1 < i) loop.break;
                val diff = arr.tail(i) - arr.head
                if (diff > 0 && diff <= 3) {
                    mem(index + i + 1) = arrange(arr.tail.slice(i, arr.tail.length), index + i + 1)
                    sum += mem(index + i + 1)
                } else {
                    loop.break;
                }
            }
        }
        return sum
    }
}
println(arrange(arr, 0))