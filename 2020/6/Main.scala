import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "input.txt"

val answers : ListBuffer[(String, Int)] = ListBuffer()

var answer : String = ""
var noGroups : Int = 0
for (line <- Source.fromFile(filename).getLines) {
    if (line == "") {
        answers += ((answer, noGroups))
        answer = ""
        noGroups = 0
    } else {
        noGroups += 1
        answer += line
    }
}
answers += ((answer, noGroups))

val chars = answers.map{x => (Set() ++ x._1.split("")).toList}
val sum = chars.map{x => x.length}.reduce((x, y) => x + y)
println(sum)

// Part 2
val counts = (answers zip chars).map{z => z._2.map{
    char => (z._1._1.count(_ == char.head) == z._1._2)
}.foldLeft(0) {(acc, b) => acc + (if (b == true) 1 else 0 )}}
println(counts.sum)


