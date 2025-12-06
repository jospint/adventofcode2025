import scala.io.Source

@main def Day4Part1(fileName: String): Unit = {
  def calculateAccessibleRolls(line: String, prevLine: String, nextLine: String): Int = {
    Range(1, line.length - 1).map { colIndex =>
      val isARoll = line(colIndex) == '@'
      val hasAccessibleRolls = (prevLine.slice(colIndex - 1, colIndex + 2) ++ line(colIndex - 1).toString ++ line(colIndex + 1).toString ++ nextLine.slice(colIndex - 1, colIndex + 2)).count(_ == '@') < 4
      isARoll && hasAccessibleRolls
    }.count(identity)
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toArray.map("." ++ _ ++ ".")
  val lineSize = lines(0).length
  val accessibleRolls = Range(0, lines.length).map { lineIndex =>
    if (lineIndex == 0) {
      calculateAccessibleRolls(lines(lineIndex), ".".repeat(lineSize), lines(lineIndex + 1))
    } else if (lineIndex == lines.length - 1) {
      calculateAccessibleRolls(lines(lineIndex), lines(lineIndex - 1), ".".repeat(lineSize))
    } else {
      calculateAccessibleRolls(lines(lineIndex), lines(lineIndex - 1), lines(lineIndex + 1))
    }
  }.sum
  println(s"The total accessible rolls are $accessibleRolls")

  source.close()
}

