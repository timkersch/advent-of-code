import scala.io.Source

val filename = "input.txt"
val input : Array[(String, String)] = Source.fromFile(filename).getLines.toArray.map{x => x.splitAt(7)}

def toBinary(str: String) : Int = {
    val binary = str.replaceAll("F", "0").replaceAll("B", "1").replaceAll("L", "0").replaceAll("R", "1")
    Integer.parseInt(binary, 2)
}

val seatMap = input.map{case (row: String, col: String) => (toBinary(row), toBinary(col))}
val seatIds = seatMap.map{case (row: Int, col: Int) => row * 8 + col}

val maxSeatId = seatIds.reduce((x, y) => x max y)
println(maxSeatId)

// Part 2
def getMissing(arr: Array[Int]) : Int = {
    if (arr.head + 1 != arr.tail(0)) arr.head + 1
    else getMissing(arr.tail)
}

val missing = getMissing(seatIds.sorted)
println(missing)
