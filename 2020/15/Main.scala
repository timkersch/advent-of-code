import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray
val startingNumbers = input(0).split(",").map{x => x.toInt}

def initMapping(len: Int) : Array[Int] = {
    var mapping = Array.fill[Int](len)(-1)
    ((0 until startingNumbers.length-1) zip startingNumbers.slice(0, startingNumbers.length-1)).foreach{case (index: Int, number: Int) => mapping(number) = index}
    return mapping
}

def getLastnumber(last: Int, startingNumbers: Array[Int], mapping: Array[Int]) : Int = {
    var lastNumber = startingNumbers(startingNumbers.length-1)
    (startingNumbers.length until last).foreach{x => {
        if (mapping(lastNumber) != -1) {
            val last = mapping(lastNumber)
            mapping(lastNumber) = x - 1 
            lastNumber = x - 1 - last
        } else {
            mapping(lastNumber) = x - 1 
            lastNumber = 0
        }
    }}
    return lastNumber
}

// Part 1
println(getLastnumber(2020, startingNumbers, initMapping(2020)))

// Part 2
println(getLastnumber(30000000, startingNumbers, initMapping(30000000)))