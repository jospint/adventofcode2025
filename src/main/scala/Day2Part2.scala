import scala.collection.immutable.NumericRange
import scala.io.Source

@main def Day2Part2(fileName: String): Unit = {
  def calculateInvalidIds(sequence: String): Seq[String] = {
    val ids = sequence.split('-').map(_.trim).map(_.toLong).toSeq
    NumericRange.inclusive[Long](ids.head, ids.tail.head, 1).flatMap(id => Some(id).filter(isInvalidId)).map(_.toString)
  }

  def isInvalidId(number: Long): Boolean = {
    val divisibleNumbers = findDivisibleNumbers(number.toString.length)
    divisibleNumbers.exists(divisibleNumber => number.toString.grouped(divisibleNumber).distinct.size == 1)
  }

  def findDivisibleNumbers(length: Int): Seq[Int] = {
    Range.inclusive(1, length / 2).flatMap(number => Some(number).filter(length % _ == 0))
  }
  
  val source = Source.fromFile(fileName)
  val strBuilder = new StringBuilder()
  val sequence = strBuilder.appendAll(source.toList).result().split(",").toSeq
  val finalCombination = sequence.flatMap(calculateInvalidIds).foldLeft(0L)((acc, id) => acc + id.toLong)
  println(s"The final combination is: $finalCombination")
  source.close()
}


