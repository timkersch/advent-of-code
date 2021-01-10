import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray.map{x => x.toLong}

def getLoopSize(publicKey: Long) : Int = {
    val subjectNumber = 7L
    val div = 20201227L
    var v = 1L
    var count = 0
    do {
        v = v * subjectNumber
        v = v % div
        count += 1
    } while(v != publicKey)
    return count
}

def getKey(iterations: Int, subjectNumber: Long) : Long = {
    val div = 20201227L
    var v = 1L
    for (i <- 0 until iterations) {
        v = v * subjectNumber
        v = v % div
    }
    return v
}

val doorLoopSize = getLoopSize(input(1))
val key = getKey(doorLoopSize, input(0))
println(key)