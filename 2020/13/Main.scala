import scala.io.Source

val filename = "input.txt"
val input : Array[String] = Source.fromFile(filename).getLines.toArray

val timestamp = input(0).toInt
val buses = input(1).split(",").filter(x => x!="x").map(x => x.toInt)

val divisions = buses.map{x => ((timestamp / x) * x) + x - timestamp}
val waitingTime = divisions.min
val busId = buses(divisions.indexOf(waitingTime))

println(busId * waitingTime)

// Part 2
val arr = input(1).split(",")
val range = 0 until arr.length
val busesPt2 = (arr zip range).filter(x => x._1 != "x").map(x => (x._1.toInt, x._1.toInt - x._2))
val N = busesPt2.map{case (x, y) => BigInt(x)}.product

val gcds = busesPt2.map{x => gcdExtended(N/BigInt(x._1), BigInt(x._1))}

val res = (gcds zip busesPt2).map{case (s, b) => s*b._2 * N/b._1}
println(res.sum % N)

def gcdExtended(a1: BigInt, m1: BigInt) : (BigInt) = {
    var m = m1
    var m0 = m
    var y = BigInt(0)
    var x = BigInt(1)
    var a = a1

    if (m == 1) {
        return BigInt(0)
    }

    while (a > 1) {
        var q = a / m
        var t = m
        m = a % m
        a = t
        t = y

        y = x - q * y
        x = t
    }

    if (x < 0) {
        x += m0
    }
    return x
}