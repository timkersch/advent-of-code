import scala.io.Source

val filename = "input.txt"
val input : Array[String] = Source.fromFile(filename).getLines.toArray

class Point(n : Int, s: Int, e: Int, w: Int) {
    
    val N: Int = n
    val S: Int = s
    val E: Int = e
    val W: Int = w

    def this(point: Point) = this(point.N, point.S, point.E, point.W)

    def timesStep(step: Int) : Point = {
        new Point(N * step, S * step, E * step, W * step)
    }

    def addStep(step: Int, direction: Char) : Point = {
        if (direction == 'N') new Point(N + step, S, E, W)
        else if (direction == 'S') new Point(N, S + step, E, W)
        else if (direction == 'E') new Point(N, S, E + step, W)
        else new Point(N, S, E, W + step)
    }

    def addPoint(point: Point) : Point = {
        new Point(N + point.N, S + point.S, E + point.E, W + point.W)
    }

    def rotateClockwise(degrees: Int) : Point = {
        var i = 90
        var newPoint = new Point(this)
        while (i <= degrees) {
            newPoint = new Point(newPoint.W, newPoint.E, newPoint.N, newPoint.S)
            i += 90
        }
        return newPoint
    }

    def rotateCounterClockWise(degrees: Int) : Point = {
        var i = 90
        var newPoint = new Point(this)
        while (i <= degrees) {
            newPoint = new Point(newPoint.E, newPoint.W, newPoint.S, newPoint.N)
            i += 90
        }
        return newPoint
    }

    def manhattan() : Int = {
        (N-S).abs + (E-W).abs
    }
    
    def asString() : String = {
        return ("N: " + N + " - S: " + S + " - W: " + W + " - E: " + E)
    }
}

def nextDirection(currentDirection: Char, degrees: Int, step : (Char) => Char) = {
    var i = 90
    var newDirection = currentDirection
    while (i <= degrees) {
        newDirection = step(newDirection)
        i += 90
    }
    newDirection
}

def nextDirectionClockwise(currentDirection: Char) : Char = {
    currentDirection match {
        case 'E' => 'S'
        case 'S' => 'W'
        case 'W' => 'N'
        case 'N' => 'E'
    }
}


def nextDirectionCounterClockwise(currentDirection: Char) : Char = {
    currentDirection match {
        case 'S' => 'E'
        case 'E' => 'N'
        case 'N' => 'W'
        case 'W' => 'S'
    }
}

var currentDirection = 'E'
var point = new Point(0,0,0,0)
input.foreach(i => {
    val direction : Char = i(0)
    val steps : Int = i.substring(1, i.length).toInt
    if (direction == 'L') {
        currentDirection = nextDirection(currentDirection, steps, nextDirectionCounterClockwise)
    } else if (direction == 'R') {
        currentDirection = nextDirection(currentDirection, steps, nextDirectionClockwise)
    } else if (direction == 'F') {
        point = point.addStep(steps, currentDirection)
    } else {
        point = point.addStep(steps, direction)
    }
})

println(point.manhattan)

// Part 2
var marker = new Point(1,0,10,0)
var boat = new Point(0,0,0,0)
input.foreach(i => {
    val direction : Char = i(0)
    val steps : Int = i.substring(1, i.length).toInt  
    if (direction == 'L') {
        marker = marker.rotateCounterClockWise(steps)
    } else if (direction == 'R') {
        marker = marker.rotateClockwise(steps)
    } else if (direction == 'F') {
        boat = boat.addPoint(marker.timesStep(steps))
    } else {
        marker = marker.addStep(steps, direction)
    }
})

println(boat.manhattan)
