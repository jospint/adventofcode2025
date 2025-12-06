import scala.io.Source

@main def Day3Part1(fileName: String): Unit = {
  def calculateLargestVoltage(bank: String): Int = {
    val batteries = bank.map(_.toString).map(_.toInt).toArray
    val batteriesExceptLast = batteries.reverse.tail.reverse
    val max = batteriesExceptLast.max
    val indexOfMax = batteries.indexOf(max)
    val batteriesAfterMax = batteries.slice(indexOfMax + 1, batteries.length)
    val maxAfterMax = batteriesAfterMax.max
    println(s"The largest voltage is: $max$maxAfterMax")
    max * 10 + maxAfterMax
  }


  val source = Source.fromFile(fileName)
  val lines = source.getLines()
  val totalVoltage = lines.map(calculateLargestVoltage).sum
  println(s"The total voltage is: $totalVoltage")
  source.close()
}
