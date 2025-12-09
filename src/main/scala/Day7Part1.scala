import scala.annotation.tailrec
import scala.io.Source

@main def Day7Part1(fileName: String): Unit = {

  @tailrec
  def splitTachyonBeam(remainingLines: Seq[String], acc: Int): Int = {
    val currentline = remainingLines.head
    val next = remainingLines.tail
    if (next.isEmpty) {
      acc
    } else {
      val nextLine = next.head
      val beamIndexes = findAllBeams(currentline, Seq.empty, 0)
      val (toSplitPairsSeq, doesSplit) = beamIndexes.map { beamIndex =>
        nextLine(beamIndex) match {
          case '^' =>
            val splitLeftBeam = if (beamIndex - 1 >= 0 && nextLine(beamIndex - 1) == '.') Seq(beamIndex - 1) else Nil
            val splitRightBeam = if (beamIndex + 1 < nextLine.length && nextLine(beamIndex + 1) == '.') Seq(beamIndex + 1) else Nil
            val itSplits = splitLeftBeam.nonEmpty || splitRightBeam.nonEmpty
            (splitLeftBeam ++ splitRightBeam, itSplits)
          case '.' => (Seq(beamIndex), false)
        }
      }.unzip
      val toSplitPairs = toSplitPairsSeq.flatten
      val newAcc = acc + doesSplit.count(_ == true)
      val toSplit = toSplitPairs.distinct
      val newLine = toSplit.foldLeft(nextLine)((line, beamIndex) => line.updated(beamIndex, '|'))
      splitTachyonBeam(newLine +: next.tail, newAcc)
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
  val result = splitTachyonBeam(lines, 0)
  println(s"The beam splitted $result times")

  source.close()
}
