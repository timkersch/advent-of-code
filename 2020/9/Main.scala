import scala.io.Source

val filename = "input.txt"
val input : Array[Long] = Source.fromFile(filename).getLines.toArray.map{x => x.toLong}

def hasSum(arr: Array[Long], sum: Long) : Boolean = {
     if (arr.length < 2) return false
     val head = arr(0)
     val tail = arr(arr.length-1)
     if (head + tail > sum) {
         hasSum(arr.slice(0, arr.length-1), sum)
      } else if (head + tail < sum) {
          hasSum(arr.tail, sum)
      } else {
          true
      }
}

def findInvalid(arr: Array[Long]) : Long = {
    val s = arr.slice(0, 25)
    val isValid = hasSum(s.sorted, arr(25))
    if (isValid) {
        findInvalid(arr.tail)
    } else {
        arr(25)
    }
}
val invalidNumber = findInvalid(input)
println(invalidNumber)


// Part 2
def search(arr: Array[Long], num: Long) : Long = {
    var sum : Long = 0
    var i = 0
    while(sum < num) {
        sum += arr(i)
        i += 1
    }
    if (sum == num) {
        val sliced = arr.slice(0, i)
        return sliced.min + sliced.max
    } else {
        return search(arr.tail, num)
    }
}
val res = search(input, invalidNumber)
println(res)