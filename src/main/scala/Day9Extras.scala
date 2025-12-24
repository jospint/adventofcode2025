case class Tile(x: Long, y: Long) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case another: Tile => x == another.x && y == another.y
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val hash = x.hashCode() ^ y.hashCode()
    31 * hash
  }
}

case class Segment(a: Tile, b: Tile) {
  override def equals(obj: Any): Boolean = obj match {

    case another: Segment =>
      (a == another.a && b == another.b) || (a == another.b && b == another.a)
  }

  override def hashCode(): Int = {
    val hash = a.hashCode() ^ b.hashCode()
    31 * hash
  }
}

case class RectangleBoundaries(xFrom: Long, xTo: Long, yFrom: Long, yTo: Long) {

  def getSegments: Array[Segment] = {
    Array(
      Segment(Tile(xFrom, yFrom), Tile(xFrom, yTo)),
      Segment(Tile(xFrom, yFrom), Tile(xTo, yFrom)),
      Segment(Tile(xTo, yFrom), Tile(xTo, yTo)),
      Segment(Tile(xFrom, yTo), Tile(xTo, yTo)),
    )
  }

}

case class Rectangle(a: Tile, b: Tile, area: Long) {

  def getRectangleEdges: RectangleBoundaries = {
    val xC = Seq(a.x, b.x).sorted
    val yC = Seq(a.y, b.y).sorted
    RectangleBoundaries(xC.head, xC.tail.head, yC.head, yC.tail.head)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case another: Circuit => (a == another.a && b == another.b) || (a == another.b && b == another.a)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val hash = a.hashCode() ^ b.hashCode()
    31 * hash + area.hashCode()
  }
}