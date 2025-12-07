import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class FreshIngredientsIterator(count: Long, currentId: Long)

@main def Day5Part2(fileName: String): Unit = {
  def calculateAllFreshIngredients(ranges: Array[String]): Long = {
    val rangeList = ranges.map { range =>
      val rangeElems = range.split('-')
      IngredientRange(rangeElems(0).toLong, rangeElems(1).toLong)
    }.sortBy(range => range.start)
    val mergedIntervals = ArrayBuffer[IngredientRange]()
    val iterator = rangeList.iterator
    while (iterator.hasNext) {
      val interval = iterator.next()
      val contains = mergedIntervals.indexWhere(mergedInterval => mergedInterval.start <= interval.start && mergedInterval.end >= interval.end)
      if (contains != -1) {
        //DO NOTHING
      } else {
        val leftOverlap = mergedIntervals.indexWhere(mergedInterval => mergedInterval.end >= interval.start)
        if (leftOverlap != -1) {
          mergedIntervals.update(leftOverlap, IngredientRange(mergedIntervals(leftOverlap).start, interval.end))
        } else {
          mergedIntervals.addOne(interval)
        }
      }
    }
    mergedIntervals.toSeq.map(interval => interval.end - interval.start + 1).sum
  }


  val source = Source.fromFile(fileName)
  val lines = source.getLines().toArray
  val separator = lines.indexOf("")
  val (ranges, ids) = lines.splitAt(separator)
  val freshIngredients = calculateAllFreshIngredients(ranges)
  println(s"There are $freshIngredients fresh ingredients")
  source.close()
}