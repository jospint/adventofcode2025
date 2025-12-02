import scala.collection.immutable.NumericRange
import scala.io.Source

@main def Day2Part1(fileName: String): Unit = {
  val source = Source.fromFile(fileName)
  val strBuilder = new StringBuilder()
  val sequence = strBuilder.appendAll(source.toList).result().split(",").toSeq
  val finalCombination = sequence.flatMap(calculateInvalidIds).foldLeft(0L)((acc, id) => acc + id.toLong)
  println(s"The final combination is: $finalCombination")
  source.close()
}

def calculateInvalidIds(sequence: String): Seq[String] = {
  val ids = sequence.split('-').map(_.trim).map(_.toLong).toSeq
  NumericRange.inclusive[Long](ids.head, ids.tail.head, 1).flatMap(id => Some(id).filter(isInvalidId)).map(_.toString)
}

def isInvalidId(number: Long): Boolean = {
  number.toString.length % 2 == 0 && number.toString.grouped(number.toString.length / 2).distinct.size == 1
}
