import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray

val splitIndex = input.indexOf("")
val rules = input.slice(0, splitIndex)
val strings = input.slice(splitIndex+1, input.length)

val rulesMap = rules.map{x => {
    val split = x.split(":")
    (split(0).trim, split(1).trim)
}}.toMap

def buildRegexp(rule: String, map: Map[String, String], depth: Int) : String = {
    val r = rule.split("\\|")
    if (r.length == 1 && (r(0).trim.equals("\"a\"") || r(0).trim.equals("\"b\""))) {
        return r(0).replace("\"", "")
    }

    if (depth > 30) {
        return ""
    }
    
    var result = "("
    r.foreach(node => {
        val nodes = node.trim.split(" ")
        var res = ""
        nodes.foreach(a => {
            res = res + buildRegexp(map(a), map, depth+1)
        })
        if (!result.equals("(")) {
            result = result + "|" + res
        } else {
            result = result + res
        }
    })
    return result + ")"
}

val regex = new Regex(buildRegexp("0", rulesMap, 0))
val res = strings.map{s => regex.matches(s)}

println(res.map{x => if (x) 1 else 0}.sum)

// Part 2
val mutMap = scala.collection.mutable.Map(rulesMap.toSeq: _*)
mutMap("8") = "42 | 42 8"
mutMap("11") = "42 31 | 42 11 31"
val regex1 = new Regex(buildRegexp("0", mutMap.toMap, 0))
val res1 = strings.map{s => regex1.matches(s)}

println(res1.map{x => if (x) 1 else 0}.sum)
