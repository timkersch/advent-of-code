import scala.io.Source

val filename = "input.txt"
val input : Array[String] = Source.fromFile(filename).getLines.toArray

val down = (step: Int) => input.grouped(step).map(_.head).toArray
val right = (step: Int) => (0 until input.length * step by step).toArray

val res = (down: Array[String], right: Array[Int]) => (down zip right).map{case (data: String, index: Int) => if (data(index % data.length) == '#') 1 else 0}
println(res(down(1), right(3)).sum)

// Part 2
val res1 = res(down(1), right(1)).sum
val res2 = res(down(1), right(3)).sum
val res3 = res(down(1), right(5)).sum
val res4 = res(down(1), right(7)).sum
val res5 = res(down(2), right(1)).sum
println(res1 * res2 * res3 * res4 * res5)

