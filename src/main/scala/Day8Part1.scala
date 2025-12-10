import scala.annotation.tailrec
import scala.io.Source

@main def Day8Part1(fileName: String, numberConnections: Int): Unit = {

  def calculateDistance(a: JunctionBox, b: JunctionBox): Double = {
    Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2))
  }

  @tailrec
  def calculateShortestPathsInternal(box: JunctionBox, remainingBoxes: Seq[JunctionBox], shortestDistances: Seq[Circuit]): Seq[Circuit] = {
    if (remainingBoxes.isEmpty) {
      shortestDistances
    } else {
      val boxToCompare = remainingBoxes.head
      val distance = calculateDistance(box, boxToCompare)
      val circuit = Circuit(box, boxToCompare, distance)
      if (distance == 0) {
        calculateShortestPathsInternal(box, remainingBoxes.tail, shortestDistances)
      } else if (shortestDistances.length < numberConnections) {
        val newShortestDistances: Seq[Circuit] = (shortestDistances :+ circuit).sorted(using CircuitOrdering)
        calculateShortestPathsInternal(box, remainingBoxes.tail, newShortestDistances)
      } else {
        val longestDistance = shortestDistances.reverse.head.distance
        if (!shortestDistances.contains(circuit) && distance <= longestDistance) {
          val newShortestDistances: Seq[Circuit] = (shortestDistances :+ circuit).sorted(using CircuitOrdering)
          calculateShortestPathsInternal(box, remainingBoxes.tail, newShortestDistances.reverse.tail.reverse)
        } else {
          calculateShortestPathsInternal(box, remainingBoxes.tail, shortestDistances)
        }
      }
    }
  }

  @tailrec
  def calculateShortestPaths(allBoxes: Seq[JunctionBox], remainingBoxes: Seq[JunctionBox], shortestDistances: Seq[Circuit]): Seq[Circuit] = {
    if (remainingBoxes.isEmpty) {
      shortestDistances
    } else {
      val box = remainingBoxes.head
      val shortestDistancesWithBox = calculateShortestPathsInternal(box, allBoxes, shortestDistances)
      calculateShortestPaths(allBoxes, remainingBoxes.tail, shortestDistancesWithBox)
    }
  }

  @tailrec
  def makeGroups(remainingCircuits: Seq[Circuit], groups: Set[Set[JunctionBox]]): Set[Set[JunctionBox]] = {
    if (remainingCircuits.isEmpty) {
      groups
    } else {
      val circuit = remainingCircuits.head
      val containingGroup = groups.find(group => group.contains(circuit.a) || group.contains(circuit.b))
      val newGroups = if (containingGroup.isEmpty) {
        groups ++ Set(Set(circuit.a, circuit.b))
      } else {
        val aGroup = groups.find(group => group.contains(circuit.a)).getOrElse(Set())
        val bGroup = groups.find(group => group.contains(circuit.b)).getOrElse(Set())
        val newGroup = aGroup ++ bGroup ++ Set(circuit.a, circuit.b)
        groups -- Set(aGroup) -- Set(bGroup) ++ Set(newGroup)
      }
      makeGroups(remainingCircuits.tail, newGroups)
    }
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines()
  val boxes = lines.map(_.split(',')).map(xyz => JunctionBox(xyz(0).toInt, xyz(1).toInt, xyz(2).toInt)).toSeq
  val shortestCircuits = calculateShortestPaths(boxes, boxes, Seq())

  val groups = makeGroups(shortestCircuits, Set())

  val sizes: Seq[Int] = groups.map(_.size).toSeq.sorted.reverse
  val result = sizes.head * sizes.tail.head * sizes.tail.tail.head
  println(s"The result is $result")

  source.close()

}
