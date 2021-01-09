import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"

val reg = new Regex("(e)|(se)|(sw)|(w)|(nw)|(ne)")
val input = Source.fromFile(filename).getLines.toArray
val charCount = input.map{x => reg.findAllIn(x).toArray.groupBy(identity).view.mapValues(_.size).toMap}
val coords = charCount.map{x => {
    val nw = x.getOrElse("nw", 0)
    val ne = x.getOrElse("ne", 0)
    val e = x.getOrElse("e", 0)
    val w = x.getOrElse("w", 0)
    val sw = x.getOrElse("sw", 0)
    val se = x.getOrElse("se", 0)
    (ne - sw - w + e, nw - se + w - e, se - nw - ne + sw)
}}

val groupedCoordsSet = coords.groupBy(identity).view.mapValues(_.size).toSet
val groupedCoords = groupedCoordsSet.map{case (x,y) => (x, y % 2 != 0)}
println(groupedCoords.count{case (_, y) => y == true})

// Part 2
def getCoords(tile: (Int, Int, Int)) : Set[(Int, Int, Int)] = {
    Array(0,1,-1).permutations.toArray.map{x => {
        ((tile._1 + x(0), tile._2 + x(1), tile._3 + x(2)))
    }}.toSet
}

var blackTiles = groupedCoords.filter{case (x, y) => y == true}.map{case (x, y) => x}
var i = 0
while(i < 100) {
    val newBlackTiles = blackTiles.filter{x => {
        val whiteNeighbours = getCoords(x).filter{y => !blackTiles.contains(y)}
        val noBlackneighbours = 6 - whiteNeighbours.size
        !(noBlackneighbours == 0 || noBlackneighbours > 2)
    }}

    var whiteSet : Set[(Int, Int, Int)] = Set()
    blackTiles.foreach{x => {
        whiteSet = whiteSet ++ getCoords(x).filter{y => !blackTiles.contains(y)}
    }}
    val whiteToBlack = whiteSet.filter{x => {
        val whiteNeighbours = getCoords(x).filter{y => !blackTiles.contains(y)}
        val noBlackneighbours = 6 - whiteNeighbours.size
        (noBlackneighbours == 2)
    }}
    blackTiles = newBlackTiles ++ whiteToBlack
    i += 1
}

println(blackTiles.size)