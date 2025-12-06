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
  val rawLines = source.getLines().toArray
  val lineSize = rawLines(0).length
  val lines = ".".repeat(lineSize) +: rawLines.map("." ++ _ ++ ".") :+ ".".repeat(lineSize)
  val accessibleRolls = Range(1, lines.length - 1).map { lineIndex =>
    calculateAccessibleRolls(lines(lineIndex), lines(lineIndex - 1), lines(lineIndex + 1))
  }.sum
  println(s"The total accessible rolls are $accessibleRolls")

  source.close()
}

