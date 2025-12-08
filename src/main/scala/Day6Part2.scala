import scala.annotation.tailrec
import scala.io.Source

@main def Day6Part2(fileName: String): Unit = {
  @tailrec
  def calculateGroupLengths(remaining: String, acc: Seq[Int], currentGroupSize: Int): Seq[Int] = {
    if (remaining == "") {
      acc :+ (currentGroupSize + 1)
    } else {
      remaining.head match {
        case x if Seq('*', '+').contains(x) =>
          if (currentGroupSize != 0) {
            calculateGroupLengths(remaining.tail, acc :+ currentGroupSize, 0)
          } else {
            calculateGroupLengths(remaining.tail, acc, 0)
          }
        case _ =>
          calculateGroupLengths(remaining.tail, acc, currentGroupSize + 1)
      }
    }
  }

  @tailrec
  def calculateAnswer(remainingContent: Seq[String], remainingOperators: Seq[String], remainingLengths: Seq[Int], results: Seq[Long]): Long = {
    val currentOperator = remainingOperators.head
    val currentSize = remainingLengths.head
    val currentContent = remainingContent.map(_.substring(0, currentSize)).toArray
    val parsedNumbers = parseNumbers(currentContent)
    
    val result = currentOperator match {
      case "*" => parsedNumbers.product
      case "+" => parsedNumbers.sum
    }
    if (remainingOperators.tail.isEmpty) {
      (results :+ result).sum
    } else {
      calculateAnswer(
        remainingContent = remainingContent.map(_.substring(currentSize + 1)),
        remainingOperators = remainingOperators.tail,
        remainingLengths = remainingLengths.tail,
        results = results :+ result
      )
    }
  }

  def parseNumbers(numbers: Array[String]): Seq[Long] = {
    Range.inclusive(0, numbers.head.length - 1).map(index => numbers.map(number => number(index)).mkString.trim.toLong)
  }

  def fillNumber(input: Array[Char], maxLength: Int): Array[Char] = {
    if (input.length == maxLength) input
    else input ++ List.fill(maxLength - input.length)('0').toArray
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toSeq
  val operatorsLine = lines.reverse.head
  val groupLengths = calculateGroupLengths(operatorsLine, List(), 0)
  val operators = operatorsLine.split(' ').toSeq.filterNot(_ == "")
  
  val answer = calculateAnswer(lines.reverse.tail.reverse, operators, groupLengths, Seq())
  println(s"The answer is $answer")
  source.close()
}
