import scala.annotation.tailrec
import scala.io.Source


@main def Day3Part2(fileName: String): Unit = {
  def calculateLargestVoltage(bank: String): String = calculateLargestVoltageRec(bank, 12, "")

  @tailrec
  def calculateLargestVoltageRec(bank: String, batteriesLeft: Int, acc: String): String = {
    batteriesLeft match {
      case 1 =>
        val batteries = bank.map(_.toString).map(_.toInt).toArray
        val max = batteries.max
        val indexOfMax = batteries.indexOf(max)
        val largestVoltage = acc ++ max.toString
        println(s"The largest voltage is: $largestVoltage")
        largestVoltage
      case _ =>
        val batteries = bank.map(_.toString).map(_.toInt).toArray
        val max = batteries.slice(0, batteries.length - batteriesLeft + 1).max
        val indexOfMax = batteries.indexOf(max)
        val newBank = batteries.drop(indexOfMax + 1).map(_.toString).mkString
        calculateLargestVoltageRec(newBank, batteriesLeft - 1, acc ++ max.toString)
    }
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toList
  val totalVoltage = lines.map(calculateLargestVoltage).map(_.toLong).sum
  println(s"The total voltage is: $totalVoltage")
  source.close()
}
