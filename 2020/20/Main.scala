import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray.filter(x => !x.equals(""))
val tiles = input.grouped(11).toArray

val tilesMap = tiles.map{x => (x(0).split(" ")(1).split(":")(0).toInt, x.slice(1, x.length))}.toMap
val cornerTiles = tilesMap.map{case (x, arr) => (x, getCorners(arr))}

def getCorners(tile: Array[String]) : Array[String] = {
    val topRow = tile(0)
    val lastRow = tile(tile.length-1)
    val transposedTile = tile.map{x => x.split("")}.transpose.map{x => x.mkString("")}
    val leftColumn = transposedTile(0)
    val rightColumn = transposedTile(transposedTile.length-1)
    return Array(topRow, topRow.reverse, lastRow, lastRow.reverse, leftColumn, leftColumn.reverse, rightColumn, rightColumn.reverse)
}

def isCorner(tile: Array[String], arr: Array[String]) : Boolean = {
    val topRow = tile(0)
    val lastRow = tile(tile.length-1)
    val transposedTile = tile.map{x => x.split("")}.transpose.map{x => x.mkString("")}
    val leftColumn = transposedTile(0)
    val rightColumn = transposedTile(transposedTile.length-1)
    
    val containsTop = arr.count(x => x.equals(topRow)) >= 2
    val containsLast = arr.count(x => x.equals(lastRow)) >= 2
    val containsLeft = arr.count(x => x.equals(leftColumn)) >= 2
    val containsRight = arr.count(x => x.equals(rightColumn)) >= 2

    return ((!containsTop && !containsLeft) || (!containsTop && !containsRight) || (!containsLast && !containsLeft) || (!containsLast && !containsRight))
}

val res = tilesMap.keys.filter(k => {
    isCorner(tilesMap(k), cornerTiles.map{case(x, arr) => arr}.flatMap{x => x}.toArray)
}).toSet

println(res.map{x => x.toLong}.product)

// Part 2
val allRotations = tilesMap.map{case (k, v) => (k, getRotations(v))}.toMap
val allCorners = tilesMap.filter{case (k, v) => !res.contains(k)}.map{case (x, arr) => getCorners(arr)}.toArray
val candidateCorners = (res zip ((res map tilesMap).toArray.map{x => getRotations(x)})).toMap

val upperLeftId = candidateCorners.head._1
val upperLeftValue = candidateCorners.head._2.filter{x => isLeftCorner(x, allCorners.flatMap{x => x})}.head
var image = Array.ofDim[(Int, Array[String])](Math.sqrt(tilesMap.size).toInt, Math.sqrt(tilesMap.size).toInt)
var added = Set(upperLeftId)

image(0)(0) = (upperLeftId, upperLeftValue)

var i = 0
var j = 0
while(i < Math.sqrt(tilesMap.size).toInt) {
    val prevX = image(i)(0)
    val matchingRight = findMatchingIdAndRotationRight(added, allRotations, prevX._2)
    if (matchingRight.size == 1) {
        added = added ++ Set(matchingRight.head._1)
        image(i+1)(0) = matchingRight.toArray.head
    }
    j = 1
    while(j < Math.sqrt(tilesMap.size).toInt) {
        val prevY = image(i)(j-1)
        val matchingBottom = findMatchingIdAndRotationBottom(added, allRotations, prevY._2)
        if (matchingBottom.size == 1) {
            added = added ++ Set(matchingBottom.head._1)
            image(i)(j) = matchingBottom.toArray.head
        }
        j += 1
    }
    i += 1
}

val removedCorners = image.map{x => x.map{case (a,b) => b}.map{x => removeCorners(x)}}
val flattened = removedCorners.map{x => x.flatMap{y => y}}.reduce((a,b) => (a zip b).map{case (x,y) => x + y})

val pattern = new Regex("..................#.#....##....##....###.#..#..#..#..#..#...")
val rots = getRotations(flattened)
val ress = rots.map{x => (x, find(x, pattern))}.filter{case (x, y) => y != 0}.head
val count = ress._1.reduce((a,b) => a + b).count(x => x == '#')
println(count - (15 * ress._2))

def find(arr: Array[String], regex: Regex) : Int = {
    var monsters = 0
    for (i <- 0 until arr.length - 3) {
        val sliced = arr.slice(i, i+3)
        for (j <- 0 until arr.length - 20) {
            val substr = sliced.map{x => x.substring(j, j+20)}
            val str = substr.reduce((x,y) => x + y)
            if (regex.matches(str)) monsters += 1
        }
    }
    return monsters
}

def removeCorners(arr: Array[String]) : Array[String] = {
    val removedTopBottom = arr.slice(1, arr.length-1)
    val transposedTile = removedTopBottom.map{x => x.split("")}.transpose.map{x => x.mkString("")}
    val strippedCols = transposedTile.slice(1, arr.length-1)
    return strippedCols.map{x => x.split("")}.transpose.map{x => x.mkString("")}
}

def findMatchingIdAndRotationRight(added: Set[Int], allRotations: Map[Int, Array[Array[String]]], tile: Array[String]) : Map[Int, Array[String]] = {
    val transposedTile = tile.map{x => x.split("")}.transpose.map{x => x.mkString("")}
    val rightColumn = transposedTile(transposedTile.length-1)
    allRotations.map{case (k, v) => {
        (k, v.find{vv => {
            val transposedV = vv.map{x => x.split("")}.transpose.map{x => x.mkString("")}
            rightColumn.equals(transposedV(0)) && !added.contains(k)}
        }.getOrElse(Array()))}
    }.filter{case (k, v) => !v.isEmpty}
}

def findMatchingIdAndRotationBottom(added: Set[Int], allRotations: Map[Int, Array[Array[String]]], tile: Array[String]) : Map[Int, Array[String]] = {
    val lastRow = tile(tile.length-1)
    allRotations.map{case (k, v) => {
        (k, v.find{vv => {
            lastRow.equals(vv(0)) && !added.contains(k)}
        }.getOrElse(Array()))}
    }.filter{case (k, v) => !v.isEmpty}
}

def isLeftCorner(tile: Array[String], arr: Array[String]) : Boolean = {
    val topRow = tile(0)
    val lastRow = tile(tile.length-1)
    val transposedTile = tile.map{x => x.split("")}.transpose.map{x => x.mkString("")}
    val leftColumn = transposedTile(0)
    val rightColumn = transposedTile(transposedTile.length-1)

    val notContainsTop = arr.count(x => x.equals(topRow)) == 0
    val notContainsLeft = arr.count(x => x.equals(leftColumn)) == 0
    val containsRight = arr.count(x => x.equals(rightColumn)) >= 1
    val containsLast = arr.count(x => x.equals(lastRow)) >= 1
    return (notContainsTop && notContainsLeft && containsRight && containsLast)
}

def getRotations(tile: Array[String]) : Array[Array[String]] = {
    val flippedX = tile.map{x => x.reverse}
    val flippedY = tile.reverse
    val flippedXY = flippedX.reverse
    val transposedTile = tile.map{x => x.split("")}.transpose.map{x => x.mkString("")}
    val flippedTransposedX = transposedTile.map{x => x.reverse}
    val flippedTransposedY = transposedTile.reverse 
    val flippedTransposedXY = flippedTransposedX.reverse
    return Array(tile, flippedX, flippedY, flippedXY, transposedTile, flippedTransposedX, flippedTransposedY, flippedTransposedXY)
}