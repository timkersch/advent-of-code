import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray
val indexOfSplit = input.indexOf("")
val ranges = input.slice(0, indexOfSplit).map{str => str.split(":")(1).split("or").map{x => x.trim.split("-")}.map{case Array(f1, f2) => (f1.toInt,f2.toInt)}}
val myTickets : Array[Int] = input(indexOfSplit+2).split(",").map{x => x.toInt}
val nearbyTickers : Array[Array[Int]] = input.slice(indexOfSplit+5, input.length).map{x => x.split(",").map{y => y.toInt}}

def allInRange(ticket: Array[Int], rangeSet: Set[Int]) : Array[Int] = {
    if (ticket.length == 0) {
        return Array()
    } else if (!rangeSet.contains(ticket.head)) {
        return Array(ticket.head) ++ allInRange(ticket.tail, rangeSet)
    } else {
        return allInRange(ticket.tail, rangeSet)
    }
}

val rangeSet = ranges.flatMap{x => x}.flatMap{case (x,y) => (x to y).toArray}.toSet
println(nearbyTickers.flatMap{x => allInRange(x, rangeSet)}.sum)

// Part 2
val rangeSets = ranges.map{case Array(f1, f2) => ((f1._1 to f1._2).toArray ++ (f2._1 to f2._2).toArray).toSet}
val validTicketColumns = nearbyTickers.filter(x => allInRange(x, rangeSet).length == 0).transpose

def inRange(ticket: Array[Int], set: Set[Int]) : Boolean = {
    if (ticket.length == 0) {
        return true
    } else if (!set.contains(ticket.head)) {
        return false
    } else {
        return inRange(ticket.tail, set)
    }
}

def getMapping(columns: Array[Array[Int]], sets: Array[Set[Int]]) : Array[(Int, List[Int])] = {
    val setIdToColumns : Array[List[Int]] = Array.fill[List[Int]](columns.length)(List())
    var setIndex = 0
    sets.foreach(set => {
        var ticketIndex = 0
        columns.foreach(ticketColumn=> {
            if (inRange(ticketColumn, set)) {
                setIdToColumns(setIndex) = setIdToColumns(setIndex) ++ List(ticketIndex)
            }
            ticketIndex += 1
        })
        setIndex += 1
    })
    ((0 until setIdToColumns.length).toArray zip setIdToColumns)
}

val setIdToColumns = getMapping(validTicketColumns, rangeSets)

var remove : List[Int] = List()
val resMapping = setIdToColumns.sortWith((x, y) => x._2.length < y._2.length).map{case (setId, columns) => {
    val remainingColumn = columns.diff(remove)
    remove = remove ++ remainingColumn
    (setId, remainingColumn(0))
}}

val indices = resMapping.filter{case (setId, column) => setId < 6}.map{case (setId, column) => column}
println(indices.map{x => myTickets(x).toLong}.product)
