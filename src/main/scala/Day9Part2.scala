import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.io.Source

@main def Day9Part2(fileName: String): Unit = {

  def calculateArea(a: Tile, b: Tile): Long = (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1)

  @tailrec
  def calculateMaxRectangleInternal(tile: Tile, remainingTiles: Array[Tile], allSegments: Array[Segment], maxRectangle: Option[Rectangle]): Option[Rectangle] = {
    if (remainingTiles.isEmpty) {
      maxRectangle
    } else {
      val tileToCompare = remainingTiles.head
      val area = calculateArea(tile, tileToCompare)
      val rectangle = Rectangle(tile, tileToCompare, area)
      maxRectangle match {
        case Some(currentMaxRectangle) =>
          if (currentMaxRectangle.area < rectangle.area) {
            val rectangleBoundaries = rectangle.getRectangleEdges
            val isWithinBoundaries = isRectangleWithinBoundaries(allSegments, rectangleBoundaries)
            if (isWithinBoundaries) {
              calculateMaxRectangleInternal(tile, remainingTiles.tail, allSegments, Option(rectangle))
            } else {
              calculateMaxRectangleInternal(tile, remainingTiles.tail, allSegments, maxRectangle)
            }
          } else {
            calculateMaxRectangleInternal(tile, remainingTiles.tail, allSegments, maxRectangle)
          }
        case None =>
          calculateMaxRectangleInternal(tile, remainingTiles.tail, allSegments, Option(rectangle))
      }
    }
  }

  def isRectangleWithinBoundaries(allSegments: Array[Segment], rectangleBoundaries: RectangleBoundaries): Boolean = {
    val noSegmentCollisions = !allSegments.exists(doesSegmentCollide(_, rectangleBoundaries))
    if (noSegmentCollisions) {
      rectangleBoundaries.getSegments.forall { boundarySegment => isSegmentWithinBoundaries(boundarySegment, allSegments) }
    } else false
  }

  def isSegmentInsideSegments(segment: Segment, allSegments: Array[Segment]): Boolean = {
    val result = allSegments.exists { segmentToCheck =>
      if (segmentToCheck.a.x == segment.b.x && segment.a.x == segment.b.x) { // Is vertical
        val minSegment = Math.min(segment.a.y, segment.b.y)
        val maxSegment = Math.max(segment.a.y, segment.b.y)
        val minSegmentToCheck = Math.min(segmentToCheck.a.y, segmentToCheck.b.y)
        val maxSegmentToCheck = Math.max(segmentToCheck.a.y, segmentToCheck.b.y)
        minSegmentToCheck <= minSegment && maxSegmentToCheck >= maxSegment
      } else if (segmentToCheck.a.y == segment.b.y && segment.a.y == segment.b.y) { // Is horizontal
        val minSegment = Math.min(segment.a.x, segment.b.x)
        val maxSegment = Math.max(segment.a.x, segment.b.x)
        val minSegmentToCheck = Math.min(segmentToCheck.a.x, segmentToCheck.b.x)
        val maxSegmentToCheck = Math.max(segmentToCheck.a.x, segmentToCheck.b.x)
        minSegmentToCheck <= minSegment && maxSegmentToCheck >= maxSegment
      } else {
        false
      }
    }
    result
  }

  def isSegmentWithinBoundaries(segment: Segment, allSegments: Array[Segment]): Boolean = {
    if (segment.a.x == segment.b.x) { // Is vertical
      val segmentsAboveToMerge = mergeVerticalSegments(allSegments.filter(segmentToFilter => segmentToFilter.a.x == segmentToFilter.b.x && segmentToFilter.a.x <= segment.a.x).sortBy(segment => Math.min(segment.a.y, segment.b.y)), segment.a.x)
      val segmentsBelowToMerge = mergeVerticalSegments(allSegments.filter(segmentToFilter => segmentToFilter.a.x == segmentToFilter.b.x && segmentToFilter.a.x >= segment.a.x).sortBy(segment => Math.min(segment.a.y, segment.b.y)), segment.a.x)
      isSegmentInsideSegments(segment, segmentsAboveToMerge) && isSegmentInsideSegments(segment, segmentsBelowToMerge)
    } else { // Is horizontal
      val segmentsAboveToMerge = mergeHorizontalSegments(allSegments.filter(segmentToFilter => segmentToFilter.a.y == segmentToFilter.b.y && segmentToFilter.a.y <= segment.a.y).sortBy(segment => Math.min(segment.a.x, segment.b.x)), segment.a.y)
      val segmentsBelowToMerge = mergeHorizontalSegments(allSegments.filter(segmentToFilter => segmentToFilter.a.y == segmentToFilter.b.y && segmentToFilter.a.y >= segment.a.y).sortBy(segment => Math.min(segment.a.x, segment.b.x)), segment.a.y)
      isSegmentInsideSegments(segment, segmentsAboveToMerge) && isSegmentInsideSegments(segment, segmentsBelowToMerge)
    }
  }

  def mergeVerticalSegments(segments: Array[Segment], xIndex: Long): Array[Segment] = {
    val head = Array(segments.head).map(segment => Segment(Tile(xIndex, segment.a.y), Tile(xIndex, segment.b.y)))
    segments.tail.foldLeft(head) { (acc, segment) =>
      val currentTailSegment = acc.reverse.head
      if (Math.max(currentTailSegment.a.y, currentTailSegment.b.y) + 1 < Math.min(segment.a.y, segment.b.y)) {
        acc :+ segment
      } else {
        acc.reverse.tail.reverse :+ Segment(Tile(xIndex, Math.min(currentTailSegment.a.y, currentTailSegment.b.y)), Tile(xIndex, Math.max(Math.max(segment.a.y, segment.b.y), Math.max(currentTailSegment.a.y, currentTailSegment.b.y))))
      }
    }
  }

  def mergeHorizontalSegments(segments: Array[Segment], yIndex: Long): Array[Segment] = {
    val head = Array(segments.head).map(segment => Segment(Tile(segment.a.x, yIndex), Tile(segment.b.x, yIndex)))
    segments.tail.foldLeft(head) { (acc, segment) =>
      val currentTailSegment = acc.reverse.head
      if (Math.max(currentTailSegment.a.x, currentTailSegment.b.x) + 1 < Math.min(segment.a.x, segment.b.x)) {
        acc :+ segment
      } else {
        acc.reverse.tail.reverse :+ Segment(Tile(Math.min(currentTailSegment.a.x, currentTailSegment.b.x), yIndex), Tile(Math.max(Math.max(segment.a.x, segment.b.x), Math.max(currentTailSegment.a.x, currentTailSegment.b.x)), yIndex))
      }
    }
  }

  def doesSegmentCollide(segment: Segment, rectangleBoundaries: RectangleBoundaries): Boolean = {
    if (segment.a.x == segment.b.x) { // Is vertical
      // Segment (a.y, b.y)
      // Boundary (xFrom, xTo)
      if (rectangleBoundaries.yFrom > Math.min(segment.a.y, segment.b.y)
        && rectangleBoundaries.yFrom < Math.max(segment.a.y, segment.b.y)
        && rectangleBoundaries.xFrom < segment.a.x
        && rectangleBoundaries.xTo > segment.a.x
      ) {
        true
      } else if (
        rectangleBoundaries.yTo > Math.min(segment.a.y, segment.b.y)
          && rectangleBoundaries.yTo < Math.max(segment.a.y, segment.b.y)
          && rectangleBoundaries.xFrom < segment.a.x
          && rectangleBoundaries.xTo > segment.a.x
      ) {
        true
      } else {
        false
      }
    } else { // Is horizontal a.y == b.y
      // Segment (a.x, b.x)
      // Boundary (yFrom, yTo)
      if (rectangleBoundaries.xFrom > Math.min(segment.a.x, segment.b.x)
        && rectangleBoundaries.xFrom < Math.max(segment.a.x, segment.b.x)
        && rectangleBoundaries.yFrom < segment.a.y
        && rectangleBoundaries.yTo > segment.a.y
      ) {
        true
      } else if (
        rectangleBoundaries.xTo > Math.min(segment.a.x, segment.b.x)
          && rectangleBoundaries.xTo < Math.max(segment.a.x, segment.b.x)
          && rectangleBoundaries.yFrom < segment.a.y
          && rectangleBoundaries.yTo > segment.a.y
      ) {
        true
      } else {
        false
      }
    }
  }


  @tailrec
  def calculateAllSegments(remainingTiles: Array[Tile], segments: Array[Segment]): Array[Segment] = {
    if (remainingTiles.isEmpty) {
      segments
    } else {
      if (segments.isEmpty) {
        val newSegments = segments :+ Segment(remainingTiles.reverse.head, remainingTiles.head)
        calculateAllSegments(remainingTiles.tail, newSegments)
      } else {
        val newSegments = segments :+ Segment(segments.reverse.head.b, remainingTiles.head)
        calculateAllSegments(remainingTiles.tail, newSegments)
      }
    }
  }

  @tailrec
  def calculateAllTilesForSegment(fixedCoordinate: Long, currentIndex: Long, endIndex: Long, isHorizontal: Boolean, acc: HashSet[Tile]): HashSet[Tile] = {
    if (currentIndex == endIndex) {
      acc
    } else {
      val newTile = if (isHorizontal) {
        Tile(currentIndex, fixedCoordinate)
      } else {
        Tile(fixedCoordinate, currentIndex)
      }
      calculateAllTilesForSegment(fixedCoordinate, currentIndex + 1, endIndex, isHorizontal, acc + newTile)
    }
  }


  @tailrec
  def calculateMaxRectangle(allTiles: Array[Tile], remainingTiles: Array[Tile], allSegments: Array[Segment], maxRectangle: Option[Rectangle]): Option[Rectangle] = {
    if (remainingTiles.isEmpty) {
      maxRectangle
    } else {
      val tile = remainingTiles.head
      val maxRectangleWithTile = calculateMaxRectangleInternal(tile, allTiles, allSegments, maxRectangle)
      calculateMaxRectangle(allTiles, remainingTiles.tail, allSegments, maxRectangleWithTile)
    }
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines().toArray
  val tiles = lines.map(_.split(',').map(_.toInt))
    .map(coordinates => Tile(coordinates(0), coordinates(1)))
  val segments = calculateAllSegments(tiles, Array())
  val allTiles = segments.map {
    segment =>
      val isHorizontal = segment.a.y == segment.b.y
      if (isHorizontal) {
        calculateAllTilesForSegment(segment.a.y, Math.min(segment.a.x, segment.b.x) + 1, Math.max(segment.a.x, segment.b.x), isHorizontal, HashSet())
      } else {
        calculateAllTilesForSegment(segment.a.x, Math.min(segment.a.y, segment.b.y) + 1, Math.max(segment.a.y, segment.b.y), isHorizontal, HashSet())
      }
  }.foldLeft(HashSet[Tile]())((a, b) => a ++ b) ++ tiles
  val maxRectangle = calculateMaxRectangle(tiles, tiles, segments, None)
  println(s"Max rectangle has area ${maxRectangle.get.area} for coordinates (${maxRectangle.get.a.x},${maxRectangle.get.a.y}) and (${maxRectangle.get.b.x},${maxRectangle.get.b.y})")

  source.close()
}