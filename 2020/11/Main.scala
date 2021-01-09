import scala.io.Source
import scala.util.control._

val filename = "input.txt"
val input : Array[String] = Source.fromFile(filename).getLines.toArray

val directions = Array(Array(0, 1), Array(1, 0), Array(0, -1), Array(-1, 0), Array(1, -1), Array(-1, 1), Array(1, 1), Array(-1, -1))

def convert(arr: Array[String], toOccupied: (Array[String], Int, Int) => Boolean, toEmpty: (Array[String], Int, Int) => Boolean) : Array[String] = {
    return arr.zipWithIndex.map{y => new String(y._1.zipWithIndex.map{x => {
        val res = x._1 match {
            case 'L' => if (toOccupied(arr, y._2, x._2)) '#' else 'L'
            case '#' => if (toEmpty(arr, y._2, x._2)) 'L' else '#'
            case _ => '.'
        }
    res}}.toArray)}
}

val converterPart1 = (input: Array[String]) => convert(input, toOccupied, toEmpty)

def converge(input: Array[String], convert: (Array[String]) => Array[String]) : Array[String] = {
    var equals = false
    var prev = input
    while (!equals) {
        val next = convert(prev)
        equals = next.sameElements(prev)
        prev = next
    }
    return prev
}

val countOccupied = (input : Array[String]) => input.foldLeft(0)  {
    (a, i) => a + i.foldLeft(0) {
        (acc, ii) => acc + (if (ii == '#') 1 else 0)
    }
}

val inRange = (arr: Array[String], y: Int, x: Int) => (x >= 0) && (y >= 0) && (y < arr.length) && (x < arr(y).length)

def toOccupied(arr : Array[String], y: Int, x: Int) : Boolean = {
    var canBeOccupied = true
    for (i <- 0 until directions.length) {
        val xDiff = directions(i)(0)
        val yDiff = directions(i)(1)
        if (inRange(arr, y + yDiff, x + xDiff)) {
            canBeOccupied = canBeOccupied && arr(y + yDiff)(x + xDiff) != '#'
        }
    }
    return canBeOccupied
}

def toEmpty(arr: Array[String], y: Int, x: Int) : Boolean = {
    var noOccupied = 0
    for (i <- 0 until directions.length) {
        val xDiff = directions(i)(0)
        val yDiff = directions(i)(1)
        if (inRange(arr, y + yDiff, x + xDiff) && arr(y + yDiff)(x + xDiff) == '#') { 
            noOccupied += 1
        }
    }
    return noOccupied >= 4
}

println(countOccupied(converge(input, converterPart1)))

// Part 2
def findFirst(arr: Array[String], yStart: Int, xStart: Int, yDir: Int, xDir: Int) : Char = {
    var x = xStart + xDir
    var y = yStart + yDir
    var current : Char = '.'
    while(inRange(arr, y, x) && current == '.') {
        current = arr(y)(x)
        x += xDir
        y += yDir
    }
    return current
}

def toOccupiedPart2(arr : Array[String], y: Int, x: Int) : Boolean = {
    var canBeOccupied = true
    for (i <- 0 until directions.length) {
        val c = findFirst(arr, y, x, directions(i)(0), directions(i)(1))
        canBeOccupied = canBeOccupied && (c != '#')
    }
    return canBeOccupied
}

def toEmptyPart2(arr : Array[String], y: Int, x: Int) : Boolean = {
    var noOccupied = 0
    for (i <- 0 until directions.length) {
        val c = findFirst(arr, y, x, directions(i)(0), directions(i)(1))
        if (c == '#') noOccupied += 1
    }
    return noOccupied >= 5
}

val converterPart2 = (input: Array[String]) => convert(input, toOccupiedPart2, toEmptyPart2)
println(countOccupied(converge(input, converterPart2)))