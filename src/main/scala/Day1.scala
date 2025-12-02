import scala.io.Source

enum Direction:
  case Left, Right

case class Rotation(direction: Direction, units: Int)

@main def Day1(fileName: String): Unit = {
  val source = Source.fromFile(fileName)
  val lines = source.getLines()
  val rotations = lines.map(extractRotation)
  val password = calculatePassword(rotations)
  println(s"The password is: $password")
  source.close()
}

def calculatePassword(rotations: Iterator[Rotation]): Int = {
  var value = 50
  var passwordCount = 0
  while (rotations.hasNext) {
    val rotation = rotations.next()
    val actualRotation = rotation.units % 100
    rotation.direction match {
      case Direction.Left => value -= actualRotation
      case Direction.Right => value += actualRotation
    }
    if (value < 0) {
      value += 100
    } else if (value >= 100) {
      value -= 100
    }
    if (value == 0) {
      passwordCount += 1
    }
    println("Current value: " + value)
  }
  passwordCount
}

def extractRotation(line: String): Rotation = {
  Rotation(line.charAt(0) match {
    case 'L' => Direction.Left
    case 'R' => Direction.Right
  }, line.substring(1).toInt)
}
