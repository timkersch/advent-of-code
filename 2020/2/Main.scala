import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"
val pattern = new Regex("(\\d+)-(\\d+) (.): (\\w*)")
val input : Array[(Int, Int, Char, String)] = Source.fromFile(filename).getLines.toArray.map{x => pattern.findAllIn(x)}.map{y => (y.group(1).toInt, y.group(2).toInt, y.group(3).head, y.group(4))}

val counts : Array[(Int, Int, Int)] = input.map{case (from, to, char, str) => (from, to, str.count(_ == char))}
val valid : Int = counts.map{case (from, to, count) => if (count >= from && count <= to) 1 else 0}.reduce((x, y) => x + y)
println(valid)

// Part 2
val counts_2 : Array[Int] = input.map{case (from, to, char, str) => if ((str(from-1) == char) ^ (str(to-1) == char)) 1 else 0}
val valid_2 : Int = counts_2.reduce((x, y) => x + y)
println(valid_2)
