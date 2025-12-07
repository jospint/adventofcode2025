import scala.io.Source

@main def Day5Part1(fileName: String): Unit = {
  def calculateFreshIngredients(ranges: Array[String], ids: Array[String]): Int = {
    val rangeList = ranges.map { range =>
      val rangeElems = range.split('-')
      IngredientRange(rangeElems(0).toLong, rangeElems(1).toLong)
    }
    val idList = ids.map(_.toLong)

    idList.map { id => rangeList.exists(range => id >= range.start && id <= range.end) }.count(identity)
  }


  val source = Source.fromFile(fileName)
  val lines = source.getLines().toArray
  val separator = lines.indexOf("")
  val (ranges, ids) = lines.splitAt(separator)
  val freshIngredients = calculateFreshIngredients(ranges, ids.tail)
  println(s"There are $freshIngredients fresh ingredients")
  source.close()
}

