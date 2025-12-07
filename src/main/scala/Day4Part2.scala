import scala.annotation.tailrec
import scala.io.Source

@main def Day4Part2(fileName: String): Unit = {

  @tailrec
  def accessRolls(lines: Array[String], previousAcessibleRolls: Int): Int = {
    val (accessedRollsList, newLines) = Range(0, lines.length).map { lineIndex =>
      if (lineIndex == 0 || lineIndex == lines.length - 1) {
        (0, lines(lineIndex))
      } else {
        calculateAccessibleRolls(lines(lineIndex), lines(lineIndex - 1), lines(lineIndex + 1))
      }
    }.unzip
    val accessibleRolls = accessedRollsList.sum
    if (accessibleRolls > 0) {
      accessRolls(newLines.toArray, previousAcessibleRolls + accessibleRolls)
    } else {
      previousAcessibleRolls
    }
  }


  def calculateAccessibleRolls(line: String, prevLine: String, nextLine: String): (Int, String) = {
    val (pickRolledList, newStateList) = Range(0, line.length).map { colIndex =>
      val state = line(colIndex)
      if (colIndex == 0 || colIndex == line.length - 1) {
        (false, state)
      } else {
        val isARoll = state == '@'
        val hasAccessibleRolls = (prevLine.slice(colIndex - 1, colIndex + 2) ++ line(colIndex - 1).toString ++ line(colIndex + 1).toString ++ nextLine.slice(colIndex - 1, colIndex + 2)).count(_ == '@') < 4
        val pickRoll = isARoll && hasAccessibleRolls
        val newState = if (pickRoll) 'x' else state
        (pickRoll, newState)
      }
    }.unzip
    val pickedRolles = pickRolledList.count(identity)
    val newLine = newStateList.mkString
    (pickedRolles, newLine)
  }

  val source = Source.fromFile(fileName)
  val rawLines = source.getLines().toArray
  val lineSize = rawLines(0).length
  val lines = ".".repeat(lineSize + 2) +: rawLines.map("." ++ _ ++ ".") :+ ".".repeat(lineSize + 2)
  val accessibleRolls = accessRolls(lines, 0)
  println(s"The total accessible rolls are $accessibleRolls")

  source.close()
}
