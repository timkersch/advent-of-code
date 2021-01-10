import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray.head
val inArr : Array[Int] = input.split("").map{x => x.toInt}

def updateCups(iterations: Int, arr: Array[Int]) : Map[Int, Int] = {
    val max = arr.max

    val zipped = (arr zip (1 until arr.length)).map{case (x, ind) => (x, arr(ind))}.toSeq
    var mapping = scala.collection.mutable.Map(zipped: _*)
    mapping.put(arr(arr.length-1), arr(0))

    var i = 0
    var currentValue = arr(0)
    while (i < iterations) {
        val first = mapping(currentValue)
        val second = mapping(first)
        val third = mapping(second)
        val nextValues = List(first, second, third)

        val destinationCup = getDestinationCup(currentValue, max, nextValues)
        val oldDestinationNext = mapping(destinationCup)
        mapping.put(destinationCup, first)
        mapping.put(currentValue, mapping(third))
        mapping.put(third, oldDestinationNext)
        
        currentValue = mapping(currentValue)
        i += 1
    }
    return mapping.toMap
}

def getDestinationCup(target: Int, max: Int, ignore: List[Int]) : Int = {
    var t = target - 1
    if (t == 0) t = max
    
    var continue = true
    while (continue) {
        if (!ignore.contains(t)) {
            continue = false
        } else {
            t = t - 1
            if (t < 1) t = max
        }
    }
    return t
}   

// Part 1
val resMappings = updateCups(100, inArr)
val buffer = new StringBuffer()
var current = resMappings(1)
while(current != 1) {
    buffer.append(current)
    current = resMappings(current)
}
println(buffer.toString)

// Part 2
val nextValue = inArr.max + 1
val additionalCups = (nextValue to 1000000).toArray
val in = inArr ++ additionalCups

val mappings = updateCups(10000000, in)
val res = mappings(1).toLong * mappings(mappings(1)).toLong
println(res)