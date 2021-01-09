import scala.io.Source

val filename = "input.txt"
val input : Array[String] = Source.fromFile(filename).getLines.toArray

def getRes(input: Array[String]): (Int, Int) = {
    val visited = Array.fill(input.length)(0)
    var acc = 0
    var i = 0
    while (i < input.length) {
        if (visited(i) == 1) {
            return (i, acc)
        } else {
            val res = exec(input(i), i, acc)
            visited(i) = 1
            acc = res._2
            i = res._1
        }
    }
    (i, acc)
}

def exec(instruction: String, currentIndex: Int, currentAcc: Int) : (Int,  Int) = {
    val splittedInstruction = instruction.split(" ")
    splittedInstruction(0) match {
        case "acc" => (currentIndex+1, currentAcc + splittedInstruction(1).toInt)
        case "nop" => (currentIndex+1, currentAcc)
        case "jmp" => (currentIndex + splittedInstruction(1).toInt, currentAcc)
    }
}

println(getRes(input)._2)

// Part 2
def swapInstruction(str: String) : String = {
    val splitted = str.split(" ")
    if (splitted(0) == "nop") {
        "jmp " + splitted(1)     
    } else if(splitted(0) == "jmp") {
       "nop " + splitted(1)
    } else {
        str
    }
}

def search(input: Array[String]) : Int = {
    for (i <- 0 until input.length) {
        val oldInput = input(i)
        val newInput = swapInstruction(oldInput)
        if (!newInput.equals(oldInput)) {
            input(i) = newInput
            val res = getRes(input)
            if (res._1 == input.length) {
                return res._2
            }
            input(i) = oldInput
        }
    }
    throw new RuntimeException("Not found")
}

println(search(input))