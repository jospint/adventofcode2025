import scala.annotation.tailrec
import scala.io.Source

@main def Day9Part1(fileName: String): Unit = {

  def calculateArea(a: Tile, b: Tile): Long = {
    (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1)
  }

  @tailrec
  def calculateMaxRectangleInternal(tile: Tile, remainingTiles: Array[Tile], maxRectangle: Option[Rectangle]): Option[Rectangle] = {
    if (remainingTiles.isEmpty) {
      maxRectangle
    } else {
      val tileToCompare = remainingTiles.head
      val area = calculateArea(tile, tileToCompare)
      val rectangle = Rectangle(tile, tileToCompare, area)
      maxRectangle match {
        case Some(currentMaxRectangle) =>
          if (currentMaxRectangle.area < rectangle.area) {
            calculateMaxRectangleInternal(tile, remainingTiles.tail, Option(rectangle))
          } else {
            calculateMaxRectangleInternal(tile, remainingTiles.tail, maxRectangle)
          }
        case None =>
          calculateMaxRectangleInternal(tile, remainingTiles.tail, Option(rectangle))
      }
    }
  }

  @tailrec
  def calculateMaxRectangle(allTiles: Array[Tile], remainingTiles: Array[Tile], maxRectangle: Option[Rectangle]): Option[Rectangle] = {
    if (remainingTiles.isEmpty) {
      maxRectangle
    } else {
      val tile = remainingTiles.head
      val maxRectangleWithTile = calculateMaxRectangleInternal(tile, allTiles, maxRectangle)
      calculateMaxRectangle(allTiles, remainingTiles.tail, maxRectangleWithTile)
    }
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toArray
  val tiles = lines.map(_.split(',').map(_.toInt))
    .map(coordinates => Tile(coordinates(0), coordinates(1)))
  val maxRectangle = calculateMaxRectangle(tiles, tiles, None)

  println(s"Max rectangle has area ${maxRectangle.get.area} for coordinates (${maxRectangle.get.a.x},${maxRectangle.get.a.y}) and ${maxRectangle.get.b.x},${maxRectangle.get.b.y})")

  source.close()
}