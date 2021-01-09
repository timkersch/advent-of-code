import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"

var mapping : scala.collection.mutable.Map[String, Set[String]] = scala.collection.mutable.Map()
var ingredients : List[String] = List()
val regexp = new Regex("(.*)\\(contains (.*)\\)")
Source.fromFile(filename).getLines.toArray.foreach{x => {
    val finds = regexp.findAllIn(x)
    val ing : Array[String] = finds.group(1).trim.split(" ")
    val allergens : Array[String] = finds.group(2).trim.split(", ")
    allergens.foreach(a => {
        val set = mapping.getOrElse(a, ing.toSet)
        mapping.put(a, set intersect ing.toSet)
    })
    ingredients = ingredients ++ ing
}}

def nextKey(mapping: scala.collection.mutable.Map[String, Set[String]], done: Set[String]) : String = {
    val sortedKeys = mapping.keys.toArray.filter{x => !done.contains(x)}.sortWith((x, y) => mapping(x).size < mapping(y).size)
    if (sortedKeys.length == 0) return ""
    return sortedKeys.head
}

def prune(mapping: scala.collection.mutable.Map[String, Set[String]], done: Set[String]) : scala.collection.mutable.Map[String, Set[String]] = {
    val currentKey = nextKey(mapping, done)
    if (currentKey.equals("")) return mapping
    
    val newMapping = mapping.map{case (key, value) => if (key == currentKey) (key, value) else (key, value diff mapping(currentKey))}
    val newDone = done ++ Set(currentKey)
    prune(newMapping, newDone)
}

val res = prune(mapping, Set())
val resIng = res.flatMap{case (x,y) => y}.toArray
println(ingredients.filter{x => !resIng.contains(x)}.length)

// Part 2
val keys = res.keys.toArray
val vals = res.values.toArray.flatMap{x => x}
val tups = keys zip vals
val sortedIngredients = tups.sortWith((x,y) => x._1 < y._1).foldLeft("") ((acc, i) => if (acc.equals("")) i._2 else acc + "," + i._2)
println(sortedIngredients)