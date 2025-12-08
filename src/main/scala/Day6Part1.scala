import scala.io.Source

@main def Day6Part1(fileName: String): Unit = {
  def calculateAnswer(matrix: Array[Array[String]]): Long = {
    Range.inclusive(0, matrix(0).length - 1).map { colIndex =>
      val operation = matrix(matrix.length - 1)(colIndex)
      val numbers = Range.inclusive(0, matrix.length - 2).map { lineIndex => matrix(lineIndex)(colIndex)}.map(_.toLong)
      operation match {
        case "+" => numbers.sum
        case "*" => numbers.product
      }
    }.sum
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toArray
  val matrix = lines.map(line => line.split(' ')).map(array => array.filterNot(_ == ""))
  val answer = calculateAnswer(matrix)
  println(s"The answer is $answer")
  source.close()
}
