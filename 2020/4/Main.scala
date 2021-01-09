import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

val filename = "input.txt"

val passports : ListBuffer[String] = ListBuffer()

var passport : String = ""
for (line <- Source.fromFile(filename).getLines) {
    if (line == "") {
        passports += passport
        passport = ""
    } else {
        passport += line + " "
    }
}
passports += passport

val shouldContain = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
val res : ListBuffer[Boolean] = passports.map{p => {
    val res = shouldContain.map{s => if (p.contains(s)) 1 else 0}.sum
    if (res == shouldContain.length) true else false
}}
println(res.foldLeft(0) {
    (a, i) => a + (if (i == true) 1 else 0)
})

// Part 2
val byr = (a: String) => {
    val i = a.toInt 
    i >= 1920 && i <= 2002
}
val iyr = (a: String) => {
    val i = a.toInt
    i >= 2010 && i <= 2020
}
val eyr = (a: String) => {
    val i = a.toInt
    i >= 2020 && i <= 2030
}
val hgt = (a: String) => {
    if (a.endsWith("cm")) {
        val slice = a.slice(0,a.length-2).toInt
        slice >= 150 && slice <= 193
    } else if (a.endsWith("in")) {
        val slice = a.slice(0,a.length-2).toInt
        slice >= 59 && slice <= 76
    } else {
        false
    }
}
val hcl = (a: String) => (new Regex("#[a-f0-9]{6}")).matches(a)
val ecl = (a: String) => (a equals "amb") || (a equals "blu") || (a equals "brn") || (a equals "gry") || (a equals "grn") || (a equals "oth") || (a equals "hzl")
val pid = (a: String) => (new Regex("[0-9]{9}")).matches(a)

val validated = passports.map{x => x.split(" ").map{elem => {
    val splitted = elem.split(":")
    val t = splitted(0)
    val value = splitted(1)
    t match {
        case "byr" => byr(value)
        case "iyr" => iyr(value)
        case "eyr" => eyr(value)
        case "hgt" => hgt(value)
        case "hcl" => hcl(value)
        case "ecl" => ecl(value)
        case "pid" => pid(value)
        case _ => true
    }
}}.reduce((a,b) => a == true && b == true)}

val z = validated zip res
val summed = z.foldLeft(0) {(acc, tup) => acc + (if (tup._1 && tup._2) 1 else 0)}
println(summed)
