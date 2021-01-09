import scala.io.Source
import java.util.Arrays

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toArray
val indexOfSeparator = input.indexOf("")
val player1 = input.slice(1, indexOfSeparator).map{x => x.toInt}
val player2 = input.slice(indexOfSeparator+2, input.length).map{x => x.toInt}

def play(player1: Array[Int], player2: Array[Int]) : Array[Int] = {
    if (player1.length == 0) {
        player2
    } else if (player2.length == 0) {
        player1
    } else if (player1.head > player2.head) {
        play(player1.tail ++ Array(player1.head, player2.head), player2.tail)
    } else {
        play(player1.tail, player2.tail ++ Array(player2.head, player1.head))
    }
}

val res = play(player1, player2)
val multiply = (1 to res.length).toArray.reverse

val mulRes = (res zip multiply).foldLeft(0) {
     (a,i) => a + (i._1 * i._2)
}

println(mulRes)

// Part 2
def playPt2(player1: Array[Int], player2: Array[Int], previous: Set[(Int, Int)]) : (Array[Int], Int) = {
    if (previous.contains((Arrays.hashCode(player1), Arrays.hashCode(player2)))) {
        return (player1, 1)
    } else if (player1.length == 0) {
        return (player2, 2)
    } else if (player2.length == 0) {
        return (player1, 1)
    }

    val newPrevious = Set((Arrays.hashCode(player1), Arrays.hashCode(player2))) ++ previous
    val p1Head = player1.head
    val p2Head = player2.head
    var winnerId : Int = 1
    if (player1.tail.length >= p1Head && player2.tail.length >= p2Head) {
        val (_, winner) = playPt2(player1.tail.slice(0, p1Head), 
                                  player2.tail.slice(0, p2Head), Set())
        winnerId = winner
    } else if (player1.head > player2.head) {
        winnerId = 1
    } else {
        winnerId = 2
    }

    if (winnerId == 1) {
        playPt2(player1.tail ++ Array(player1.head, player2.head), player2.tail, newPrevious)
    } else {
        playPt2(player1.tail, player2.tail ++ Array(player2.head, player1.head), newPrevious)
    }
}

val resPt2 = playPt2(player1, player2, Set())._1
val multiplyPt2 = (1 to resPt2.length).toArray.reverse

val mulResPt2 = (resPt2 zip multiplyPt2).foldLeft(0) {
     (a,i) => a + (i._1 * i._2)
}

println(mulResPt2)

