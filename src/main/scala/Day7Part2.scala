import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

@main def Day7Part2(fileName: String): Unit = {

  @tailrec
  def calculateTachyonBeamPaths(remainingLines: Seq[String], paths: mutable.Map[Int, Long]): Seq[Long] = {
    val currentline = remainingLines.head
    val next = remainingLines.tail
    if (next.isEmpty) {
      paths.values.toSeq
    } else {
      val nextLine = next.head
      val beamIndexes = findAllBeams(currentline, Seq.empty, 0)
      val toSplitPairs = beamIndexes.flatMap { beamIndex =>
        nextLine(beamIndex) match {
          case '^' =>
            val splitLeftBeam = if (beamIndex - 1 >= 0 && nextLine(beamIndex - 1) == '.') Seq(beamIndex - 1) else Nil
            val splitRightBeam = if (beamIndex + 1 < nextLine.length && nextLine(beamIndex + 1) == '.') Seq(beamIndex + 1) else Nil
            val countInBeamIndex = paths.getOrElse(beamIndex, 1L)
            if (splitLeftBeam.nonEmpty) {
              val countInLeftBeam = paths.getOrElse(beamIndex - 1, 0L)
              paths.put(beamIndex - 1, countInBeamIndex + countInLeftBeam)
            }
            if (splitRightBeam.nonEmpty) {
              val countInRightBeam = paths.getOrElse(beamIndex + 1, 0L)
              paths.put(beamIndex + 1, countInBeamIndex + countInRightBeam)
            }
            paths.remove(beamIndex)
            splitLeftBeam ++ splitRightBeam
          case '.' => Seq(beamIndex)
        }
      }
      val toSplit = toSplitPairs.distinct
      val newLine = toSplit.foldLeft(nextLine)((line, beamIndex) => line.updated(beamIndex, '|'))
      calculateTachyonBeamPaths(newLine +: next.tail, paths)
    }
  }

  @tailrec
  def findAllBeams(remainingString: String, acc: Seq[Int], currentIndex: Int): Seq[Int] = {
    val newAcc: Seq[Int] = if (Seq('S', '|').contains(remainingString.head)) {
      acc :+ currentIndex
    } else {
      acc
    }
    val remainingAfter = remainingString.tail
    if (remainingAfter.isEmpty) {
      newAcc
    } else {
      findAllBeams(remainingAfter, newAcc, currentIndex + 1)
    }
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toSeq
  val result = calculateTachyonBeamPaths(lines, mutable.Map[Int, Long]()).sum
  println(s"There are $result timelines")

  source.close()
}
