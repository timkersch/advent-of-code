import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"
var currentMask = ""
val pattern = new Regex("mem\\[(\\d+)\\]")

val input = Source.fromFile(filename).getLines.toArray
var mapping : scala.collection.mutable.Map[Long, Long] = scala.collection.mutable.Map()
input.foreach{x => {    
    if (x.startsWith("mask")) {
        currentMask = x.split("=")(1).trim
    } else {
        val value = x.split("=")(1).trim.toLong
        val address = pattern.findAllIn(x).group(1).toLong
        mapping(address) = applyMask(currentMask, value)
    }
}}


def sumMap(map: scala.collection.mutable.Map[Long, Long]) : Long = {
    var sum : Long = 0
    for ((k,v) <- map) {
        sum += v
    }
    sum
}

def applyMask(mask: String, value: Long) : Long = {
    val str = value.toBinaryString.reverse.padTo(mask.length, "0").reverse
    val bigString = (mask zip str).map{x => {
        if (x._1 == 'X') {
            x._2.toString
        } else {
            x._1.toString
        }
    }}.mkString("")
    return java.lang.Long.parseUnsignedLong(bigString, 2)
}
println(sumMap(mapping))

// Part 2

def combinations(mask: String) : List[String] = {
    if (mask.indexOf("X") == -1) {
        return List(mask)
    } 
    combinations(mask.replaceFirst("X", "0")) ++ combinations(mask.replaceFirst("X", "1"))
}

def applyMaskPt2(mask: String, value: Long) : List[Long] = {
    val str = value.toBinaryString.reverse.padTo(mask.length, "0").reverse
    val bigString = (mask zip str).map{x => {
        if (x._1 == '1') {
            '1'
        } else if (x._1 == '0') {
            x._2.toString
        } else {
            'X'
        }
    }}.mkString("")
    return combinations(bigString).map{x => java.lang.Long.parseUnsignedLong(x, 2)}
}

var mappingPt2 : scala.collection.mutable.Map[Long, Long] = scala.collection.mutable.Map()
var currentMaskPt2 = ""
input.foreach{x => {    
    if (x.startsWith("mask")) {
        currentMask = x.split("=")(1).trim
    } else {
        val value = x.split("=")(1).trim.toLong
        val address = pattern.findAllIn(x).group(1).toLong
        applyMaskPt2(currentMask, address).foreach(a => mappingPt2(a) = value)
    }
}}
println(sumMap(mappingPt2))