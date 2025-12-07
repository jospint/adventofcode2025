case class IngredientRange(start: Long, end: Long)

object IngredientRangeStartOrdering extends Ordering[IngredientRange] {
  def compare(a:IngredientRange, b:IngredientRange): Int = a.start.compare(b.start)
}

object IngredientRangeEndOrdering extends Ordering[IngredientRange] {
  def compare(a:IngredientRange, b:IngredientRange): Int = a.end.compare(b.end)
}