import scala.io.Source
import scala.util.matching.Regex

val mapFromRegex = new Regex("(\\w+ \\w+) bags")
val mapToRegex = new Regex("\\d (\\w+ \\w+)+ bags*")

var mapping : scala.collection.mutable.Map[String,List[String]] = scala.collection.mutable.Map()

val filename = "input.txt"
Source.fromFile(filename).getLines.toArray.foreach{x => {
    val splitted = x.split("contain")
    val fromBag = mapFromRegex.findAllIn(splitted(0)).group(1)
    if (!splitted(1).trim.equals("no other bags.")) {
        val toBags = splitted(1).split(", ").map{y => mapToRegex.findAllIn(y).group(1)}
        toBags.foreach(b => {
            val lst : List[String] = mapping.getOrElse(b, List())
            mapping.put(b, lst ++ List(fromBag))
        })
    }
}}

def findAll(searchFor: String) : Set[String] = {
    val lst = mapping.getOrElse(searchFor, List())
    if (lst.length == 0) {
        return Set()
    } else {
        var set : Set[String] = Set()
        lst.foreach(x => {
            set = findAll(x) ++ set ++ Set(x)
        })
        return set
    }
}

println(findAll("shiny gold").size)


// Part 2
val reg = new Regex("(\\d) (\\w+ \\w+)+ bags*")
var mapping2 : scala.collection.mutable.Map[String, Array[(Int, String)]] = scala.collection.mutable.Map()

Source.fromFile(filename).getLines.toArray.foreach{x => {
    val splitted = x.split("contain")
    val fromBag = mapFromRegex.findAllIn(splitted(0)).group(1)
    if (!splitted(1).trim.equals("no other bags.")) {
        val toBags : Array[(Int, String)] = splitted(1).split(", ").map{y => (reg.findAllIn(y).group(1).toInt, reg.findAllIn(y).group(2))}
        mapping2.put(fromBag, toBags)
    } else {
        mapping2.put(fromBag, Array())
    }
}}

def countAll(searchFor: String) : Int = {
    val lst = mapping2.getOrElse(searchFor, Array())
    if (lst.length == 0) {
        return 0
    } else {
        var sum = 0
        lst.foreach(x => {
            val count = x._1
            val str = x._2
            sum += count + (count * countAll(str))
        })
        return sum
    }
}

println(countAll("shiny gold"))