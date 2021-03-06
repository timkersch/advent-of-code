import scala.io.Source

val filename = "input.txt"
val input = Source.fromFile(filename).getLines.toArray.map{x => x.replace(" ", "")}

def evaluate(str: String) : (Long, Int) = {
    var i = 0
    var acc = 0L
    var op : (Long, Long) => Long = (a,_) => a
    while(i < str.length) {
        val nextToken = str(i)
        if (nextToken == '+') {
            op = (a: Long, b: Long) => a + b
        } else if (nextToken == '*') {
            op = (a: Long, b: Long) => a * b
        } else if (nextToken == '(') {
            val res = evaluate(str.slice(i+1, str.length))
            i += res._2
            acc = op(res._1, acc)
        } else if (nextToken == ')') {
            return (acc,i+1)
        } else {
            acc = op(nextToken.toString.toInt, acc)
        }
        i += 1
    }
    return (acc, i)
}

val res = input.map{x => evaluate(x)._1}
println(res.sum)

// Part 2
def makeKnuthString(input: String) : String = {
    val s = new StringBuilder()
    s.append("((((")
    input.foreach{x => {
        if (x == '(') {
            s.append("((((")
        } else if (x == ')') {
            s.append("))))")
        } else if (x == '*') {
            s.append(")))*(((")
        } else if (x == '+') {
            s.append("))+((")
        } else {
            s.append(x)
        }
    }}
    s.append("))))")
    s.toString
}

val knuthString = input.map{x => makeKnuthString(x)}
val resPt2 = knuthString.map{x => evaluate(x)._1}
println(resPt2.sum)