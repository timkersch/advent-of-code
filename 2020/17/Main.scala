import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray.map{x => x.toArray}

val res = (input zip (0 until input.length)).map{case (v, index) => 
    ((v zip (0 until v.length)).filter{case (vv, ind) => vv == '#'}.map{case (vv, ind) => ind}, index)}
var activeSet = res.flatMap{case (x, y) => x.map(h => (h, y, 0))}.toSet

def inactiveNeighbours(x: Int, y: Int, z: Int, s: Set[(Int, Int, Int)]) : Set[(Int, Int, Int)] = {
    return (for (xx <- x-1 to x+1 ; yy <- y-1 to y+1 ; zz <- z-1 to z+1) yield (xx,yy,zz)).toSet.filter{a => !s.contains(a)}
}

var cycle = 0
val maxCycles = 6
while (cycle < maxCycles) {
    val newActiveSet = activeSet.filter{x => {
        val coordNeighbours = inactiveNeighbours(x._1, x._2, x._3, activeSet)
        (coordNeighbours.size == 26 - 2 || coordNeighbours.size == 26 - 3)
    }}
    var inactiveSet : Set[(Int, Int, Int)] = Set()
    activeSet.foreach{x => {
        inactiveSet = inactiveSet ++ inactiveNeighbours(x._1, x._2, x._3, activeSet)
    }}
    val newInactiveSet = inactiveSet.filter{x => {
        val coordNeighbours = inactiveNeighbours(x._1, x._2, x._3, activeSet)
        (coordNeighbours.size == 26 - 2)
    }}
    activeSet = newActiveSet ++ newInactiveSet
    cycle += 1
}

println(activeSet.size)

// Part 2
var activeSetPt2 = res.flatMap{case (x, y) => x.map(h => (h, y, 0, 0))}.toSet

def inactiveNeighbours4D(x: Int, y: Int, z: Int, w: Int, s: Set[(Int, Int, Int, Int)]) : Set[(Int, Int, Int, Int)] = {
    return (for (xx <- x-1 to x+1 ; yy <- y-1 to y+1 ; zz <- z-1 to z+1 ; ww <- w-1 to w+1) yield (xx,yy,zz,ww)).toSet.filter{a => !s.contains(a)}
}

cycle = 0
while (cycle < maxCycles) {
    val newActiveSet = activeSetPt2.filter{x => {
        val coordNeighbours = inactiveNeighbours4D(x._1, x._2, x._3, x._4, activeSetPt2)
        (coordNeighbours.size == 80 - 2 || coordNeighbours.size == 80 - 3)
    }}
    var inactiveSet : Set[(Int, Int, Int, Int)] = Set()
    activeSetPt2.foreach{x => {
        inactiveSet = inactiveSet ++ inactiveNeighbours4D(x._1, x._2, x._3, x._4, activeSetPt2)
    }}
    val newInactiveSet = inactiveSet.filter{x => {
        val coordNeighbours = inactiveNeighbours4D(x._1, x._2, x._3, x._4, activeSetPt2)
        (coordNeighbours.size == 80 - 2)
    }}
    activeSetPt2 = newActiveSet ++ newInactiveSet
    cycle += 1
}

println(activeSetPt2.size)